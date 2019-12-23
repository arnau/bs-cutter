open Belt

exception Malformed of string

type label = string
type error = string
type stream = string
type character = string
type 'a parser = stream -> ('a * stream, label * error) Belt.Result.t
type 'a t = Parser of { parser : 'a parser; label : label }

let print_result result =
  match result with
  | Ok (value, _) ->
    {j|$value|j}
  | Error (label, error) ->
    {j|Error parsing $label\nUnexpected $error|j}

let set_label (Parser { parser }) new_label =
  let inner_fn input =
    match parser input with
    | Ok _ as ok -> ok
    | Error (_, err) ->
      Error (new_label, err)
  in
  Parser { parser = inner_fn; label = new_label }

let (<?>) = set_label

let get_label (Parser { label }) =
  label

let run (Parser { parser }) input =
  parser input

let bind parser fn =
  let label = "unknown" in
  let inner_fn input =
    match run parser input with
    | Error _ as err -> err
    | Ok (value, remaining) ->
      run (fn value) remaining
  in
  Parser { parser = inner_fn ; label }

let (>>=) = bind

let return x =
  let label = "unknown" in
  let inner_fn input =
    Ok (x, input)
  in
  Parser { parser = inner_fn; label }


let map parser f =
  let label = get_label parser in
  let inner_fn input =
    match run parser input with
    | Error _ as err -> err
    | Ok (value, remaining) ->
      Ok (f value, remaining)
  in
  Parser { parser = inner_fn; label }

let (|>>) = map

(* Could be implemented with [bind] but doesn't cut it for me, even less with
 * [>>=].
 *
 * let and_then parser1 parser2 =
 *    parser1
 *    |. bind (fun result1 ->
 *      parser2
 *      |. bind (fun result2 ->
 *        return (result1, result2)))
 *
*)
let and_then parser1 parser2 =
  let label1 = get_label parser1 in
  let label2 = get_label parser2 in
  let label = {j|$label1 and_then $label2|j} in
  let inner_fn input =
    match run parser1 input with
    | Error _ as err -> err
    | Ok (value1, remaining1) ->
      match run parser2 remaining1 with
      | Error _ as err -> err
      | Ok (value2, remaining2) ->
        Ok ((value1, value2), remaining2)
  in

  Parser { parser = inner_fn; label }

let (>>) = and_then

let apply fparser xparser =
  (fparser >> xparser)
  |. map (fun (f, x) -> f x)

let (<*>) = apply

let or_else parser1 parser2 =
  let label1 = get_label parser1 in
  let label2 = get_label parser2 in
  let label = {j|$label1 or_else $label2|j} in
  let inner_fn input =
    match run parser1 input with
    | Ok _ as res -> res
    | Error _ -> run parser2 input
  in

  Parser { parser = inner_fn; label }

let (<|>) = or_else

let lift2 f xp yp =
  return f <*> xp <*> yp

let rec sequence parser_list =
  let cons head tail = head :: tail in
  let consp = lift2 cons in

  match parser_list with
  | [] ->
    return []
  | head :: tail ->
    consp head (sequence tail)

let choice parser_list =
  match Array.get parser_list 0 with
  | None ->
    raise (Malformed "A choice parser must have at least one parser to choose from.")
  | Some p ->
    let rest = Array.sliceToEnd parser_list 1 in
    Array.reduce rest p or_else

let satisfy predicate label =
  let inner_fn input =
    if String.is_empty input then
      Error (label, "No more input")
    else
      let first = String.get input 0 in
      if predicate first then
        let remaining = String.sliceToEnd ~from:1 input in
        Ok (first, remaining)
      else
        Error (label, {j|Unexpected $first.|j})
  in
  Parser { parser = inner_fn; label }


let pchar input =
  let label = {j|$input|j} in
  let predicate ch = (ch = input) in
  satisfy predicate label

let any input =
  let label = {j|any of $input|j} in
  input
  |. Array.map pchar
  |. choice
  |. set_label label

let pstring str =
  let label = {j|pstring $str|j} in
  str
  |. String.to_list
  |. List.map pchar
  |. sequence
  |. map String.from_list
  |. set_label label

let rec parse_zero_or_more parser input =
  match run parser input with
  | Error _ -> ([], input)
  | Ok (value, input') ->
    let (values, remaining) =
      parse_zero_or_more parser input' in
    (value :: values, remaining)

let many parser =
  let old_label = get_label parser in
  let label = {j|many $old_label|j} in
  let inner_fn input =
    Ok (parse_zero_or_more parser input)
  in
  Parser { parser = inner_fn; label }

let many1 parser =
  let old_label = get_label parser in
  let label = {j|many $old_label|j} in
  let inner_fn input =
    match run parser input with
    | Error (_, err) ->
      Error (label, err)
    | Ok (value, input') ->
      let (values, remaining) =
        parse_zero_or_more parser input' in
      Ok (value :: values, remaining)
  in
  Parser { parser = inner_fn; label }

let opt parser =
  let some = map parser (fun x -> Some x) in
  let none = return None in
  some <|> none

let drop_and parser1 parser2 =
  (parser1 >> parser2)
  |. map (fun (_, b) -> b)

let (!>) = drop_and

let and_drop parser1 parser2 =
  (parser1 >> parser2)
  |. map (fun (a, _) -> a)

let (>!) = and_drop

let between parser1 parser2 parser3 =
  parser1
  |. drop_and parser2
  |. and_drop parser3

let sepby1 parser sep =
  let sep = drop_and sep in
  (parser >> many (sep parser))
  |. map (fun (p, plist) -> p :: plist)

let sepby parser sep =
  sepby1 parser sep <|> (return [])

let digit_char =
  let predicate = String.is_digit in
  let label = "digit" in
  satisfy predicate label

let whitespace_char =
  let predicate = String.is_whitespace in
  let label = "whitespace" in
  satisfy predicate label

let dquote_char =
  let predicate ch = (ch = "\"") in
  let label = "double quote" in
  satisfy predicate label

let pint =
  let to_int (sign, digit_list) =
    let i =
      digit_list
      |. String.from_list
      |. Int.fromString
      |. Option.getExn
    in
    match sign with
    | Some _ -> -i
    | None -> i
  in
  let digits = many1 digit_char in

  (opt (pchar "-") >> digits)
  |. map to_int
