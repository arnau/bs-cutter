open Belt

module String = struct
  include Js.String

  let is_empty str =
    String.length str = 0

  let to_array str =
    split "" str

  let to_list str =
    str
    |> to_array
    |> ArrayLabels.to_list

  let from_array arr =
    concatMany arr ""

  let from_list lst =
    String.concat "" lst
end

type error = string
type stream = string
type character = string
type 'a t = Parser of (stream -> (('a * stream), error) Belt.Result.t)

let run (Parser inner_fn) input =
  inner_fn input

let and_then parser1 parser2 =
  let inner_fn input =
    let result1 = run parser1 input in

    match result1 with
    | Error err -> Error err
    | Ok (value1, remaining1) ->
      let result2 = run parser2 remaining1 in
      match result2 with
      | Error err -> Error err
      | Ok (value2, remaining2) ->
        let value' = (value1, value2) in
        Ok (value', remaining2)
  in

  Parser inner_fn

let (>>) = and_then

let or_else parser1 parser2 =
  let inner_fn input =
    match run parser1 input with
    | Ok _ as res -> res
    | Error _ -> run parser2 input
  in
  Parser inner_fn

let (<|>) = or_else

exception Malformed of string

let map parser f =
  let inner_fn input =
    match run parser input with
    | Ok (value, remaining) ->
      Ok (f value, remaining)
    | Error err -> Error err
  in
  Parser inner_fn

let (|>>) = map

let return x =
  let inner_fn input =
    Ok (x, input)
  in
  Parser inner_fn

let apply fparser xparser =
  (fparser >> xparser)
  |. map (fun (f, x) -> f x)

let (<*>) = apply

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

let pchar ch =
  let innerFn str =
    if String.is_empty str then
      Result.Error "No more input"
    else
      let first = String.get str 0 in
      if first = ch then
        let remaining = String.sliceToEnd ~from:1 str in
        Result.Ok (ch, remaining)
      else
        Result.Error {j|Expecting $ch. Got $first|j}
  in
  Parser innerFn

let any input =
  input
  |. Array.map pchar
  |. choice

let pstring str =
  str
  |. String.to_list
  |. List.map pchar
  |. sequence
  |. map String.from_list

let rec parse_zero_or_more parser input =
  match run parser input with
  | Error _ -> ([], input)
  | Ok (value, input') ->
    let (values, remaining) =
      parse_zero_or_more parser input' in
    (value :: values, remaining)

let many parser =
  let rec inner_fn input =
    Ok (parse_zero_or_more parser input)
  in
  Parser inner_fn

let many1 parser =
  let inner_fn input =
    match run parser input with
    | Error _ as err -> err
    | Ok (value, input') ->
      let (values, remaining) =
        parse_zero_or_more parser input' in
      Ok (value :: values, remaining)
  in
  Parser inner_fn

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

let pdigit =
  any [|"0";"1";"2";"3";"4";"5";"6";"7";"8";"9";|]

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
  let digits = many1 pdigit in

  (opt (pchar "-") >> digits)
  |. map to_int
