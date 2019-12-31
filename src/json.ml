module P = Parser
module MapStr = Belt.Map.String

type 'a dict = 'a MapStr.t

type t =
  | String of string
  | Number of float
  | Bool of bool
  | Null
  | Object of t dict
  | Array of t list

let jnull =
  P.pstring "null"
  |. P.replace Null
  |. P.set_label "null"

let jbool =
  let open Parser in
  let jtrue = pstring "true" |. replace (Bool true) in
  let jfalse = pstring "false" |. replace (Bool false) in

  jtrue
  |. or_else jfalse
  |. set_label "bool"

let junescaped_char =
  let label = "char" in
  P.satisfy (fun ch -> ch <> "\\" && ch <> "\"") label

let jescaped_char =
  [|
    ("\\\"", "\"");    (* quote *)
    ("\\\\", "\\");    (* reverse solidus *)
    ("\\/", "/");      (* solidus *)
    ("\\b", "\b");     (* backspace *)
    ("\\f", {j|\f|j}); (* formfeed *)
    ("\\n", "\n");     (* newline *)
    ("\\r", "\r");     (* carriage return *)
    ("\\t", "\t");     (* horizontal tab *)
  |]
  |. Belt.Array.map (fun (to_match, token) ->
      P.pstring to_match |. P.replace token)
  |. P.choice
  |. P.set_label "escaped char"

external to_int : string -> int = "Number" [@@bs.val]
external to_float : string -> float = "Number" [@@bs.val]

let junicode_char =
  let open Parser in
  let backslash = pchar "\\" in
  let uchar = pchar "u" in
  let hexdigit = any (String.to_array "0123456789ABCDEFabcdef") in
  let to_char (((h1, h2), h3), h4) =
    {j|0x$h1$h2$h3$h4|j}
    |. to_int
    |. Js.String.fromCodePoint
  in
  backslash
  |. drop_and uchar
  |. drop_and hexdigit
  |. and_then hexdigit
  |. and_then hexdigit
  |. and_then hexdigit
  |. map to_char

let qstring =
  let open Parser in
  let quote = pchar "\"" |. set_label "quote" in
  let jchar =
    junescaped_char 
    |. or_else jescaped_char
    |. or_else junicode_char
  in
  quote
  |. drop_and (many jchar)
  |. and_drop quote
  |. map String.from_list

let jstring =
  let open Parser in
  qstring
  |. map (fun x -> String x)
  |. set_label "quoted string"

let jnumber =
  let open Parser in

  let opt_sign =
    opt (pchar "-") in
  let zero =
    pstring "0" in
  let digit_one_nine =
    satisfy (fun ch -> String.is_digit ch && ch <> "0") "1-9" in
  let digit =
    satisfy (fun ch -> String.is_digit ch) "digit" in
  let point =
    pchar "." in
  let e =
    pchar "e"
    |. or_else (pchar "E") in
  let opt_plus_minus =
    opt (pchar "-" |. or_else (pchar "+")) in
  let non_zero =
    digit_one_nine
    |. and_then (many digit)
    |. map (fun (first, rest) -> String.from_list (first :: rest)) in
  let int_part =
    zero
    |. or_else non_zero in
  let fraction_part =
    point
    |. drop_and (many1 digit) in
  let exponent_part =
    e
    |. drop_and opt_plus_minus
    |. and_then (many1 digit) in

  let to_jnumber (((sign, ipart), fpart), epart) =
    let sign_str =
      Belt.Option.getWithDefault sign "" in
    let fpart_str =
      fpart
      |. Belt.Option.map (fun digits ->
          let digits' =
            String.from_list digits in
          {j|.$digits'|j})
      |. Belt.Option.getWithDefault ""
    in
    let epart_str =
      epart
      |. Belt.Option.map (fun (osign, digits) ->
          let sign =
            Belt.Option.getWithDefault osign "" in
          let digits' =
            String.from_list digits in
          {j|e$sign$digits'|j}
        )
      |. Belt.Option.getWithDefault ""
    in
    {j|$sign_str$ipart$fpart_str$epart_str|j}
    |. to_float
    |. (fun x -> Number x)
  in

  opt_sign
  |. and_then int_part
  |. and_then (opt fraction_part)
  |. and_then (opt exponent_part)
  |. map to_jnumber
  |. set_label "number"

let create_parser_fwd_to_ref =
  let open Parser in
  let dummy_parser =
    let inner_fn input =
      failwith "unfixxed forwarded parser" in
    Parser {parser = inner_fn; label = "unknown"}
  in
  let parser_ref = ref dummy_parser in
  let inner_fn input =
    run_input !parser_ref input in
  let wrapper_parser = Parser {parser = inner_fn; label = "unknown"} in

  wrapper_parser, parser_ref

let jvalue, jvalue_ref = create_parser_fwd_to_ref

let jarray =
  let open Parser in
  let left = pchar "[" |. and_drop spaces in
  let right = pchar "]" |. and_drop spaces in
  let comma = pchar "," |. and_drop spaces in
  let value = jvalue |. and_drop spaces in
  let values = sepby1 value comma in
  between left values right
  |. map (fun x -> Array x)
  |. set_label "array"

let jobject =
  let open Parser in
  let left = pchar "{" |. and_drop spaces in
  let right = pchar "}" |. and_drop spaces in
  let colon = pchar ":" |. and_drop spaces in
  let comma = pchar "," |. and_drop spaces in
  let key = qstring |. and_drop spaces in
  let value = jvalue |. and_drop spaces in

  let key_value = (key |. and_drop colon) |. and_then value in
  let key_values = sepby1 key_value comma in

  (between left key_values right)
  |. map (fun x ->
      let d = x
              |. Belt.List.toArray
              |. MapStr.fromArray
      in
      Object d)
  |. set_label "object"
;;


jvalue_ref :=
  P.choice [|
    jnull;
    jbool;
    jnumber;
    jstring;
    jarray;
    jobject;
  |]


