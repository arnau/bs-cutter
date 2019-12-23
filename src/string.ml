include Js.String

let is_empty str =
  length str = 0

let to_array str =
  split "" str

let to_list str =
  str
  |> to_array
  |> ArrayLabels.to_list

let from_array arr =
  concatMany arr ""

let from_list lst =
  lst
  |. Belt.List.toArray
  |. from_array

(** Should be in Char module *)
let is_digit ch =
  match (length ch = 1, Belt.Int.fromString ch) with
  | (false, _) -> false
  | (_, None) -> false
  | (true, Some _) -> true

let is_whitespace ch =
  includes ch " \t\n"
