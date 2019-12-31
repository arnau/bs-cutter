type label = string

(** A parsing error. *)
type error = string

type position = {
  line : int;
  column : int;
}

type parser_position = {
  current_line : string;
  line : int;
  column : int;
}

type input_state = {
  lines : string array;
  position : position;
}

(** A stream of characters to be parsed. *)
type stream = input_state

type 'a parser_result = ('a * stream, label * error * parser_position) Belt.Result.t

type 'a parser = stream -> 'a parser_result

(** A character. Not using [char] because they are not utf-8 aware. *)
type character = string

(** A parser is a function of an input stream and results in a pair of the
    parsed fraction and the remaining stream. If it fails, returns an [Error]. *)
type 'a t = Parser of { parser : 'a parser; label : label }

val init_position : position

val inc_col : position -> position

val inc_line : position -> position

val from_str : string -> input_state

val current_line : input_state -> string

val next_char : input_state -> input_state * character option

val read_all_chars : input_state -> string list
val read_all_chars' : input_state -> string list

val parser_position_from_input_state : input_state -> parser_position

val set_label : 'a t -> label -> 'a t
val (<?>) : 'a t -> label -> 'a t

(** Consumes the given input with the given parser. *)
val run_input : 'a t -> stream -> 'a parser_result
val run : 'a t -> string -> 'a parser_result

(* val print_result : ('a * 'b, 'c * 'd * parser_position) result -> string *)
val print_result : 'a parser_result -> string

(** Chains the result of a parser to another parser. *)
val bind : 'a t -> ('a -> 'b t) -> 'b t
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

(** Transforms the result of a parser. *)
val map : 'a t -> ('a -> 'b) -> 'b t
val (|>>) : 'a t -> ('a -> 'b) -> 'b t

val replace : 'a t -> 'b -> 'b t

(** Wraps (lifts) a normal value into a parser. *)
val return : 'a -> 'a t

(** Function application for wrapped functions and wrapped values. *)
val apply : ('a -> 'b) t -> 'a t -> 'b t
val (<*>) : ('a -> 'b) t -> 'a t -> 'b t

(** Combines two parsers and returns both successfully or fails with [Error]. *)
val and_then : 'a t -> 'b t -> ('a * 'b) t
val (>>) : 'a t -> 'b t -> ('a * 'b) t

(** Applies the first parser and if it fails, applies the second. *)
val or_else : 'a t -> 'a t -> 'a t
val (<|>) : 'a t -> 'a t -> 'a t

(** Converts a list of parsers into a parser of a list. *)
val sequence : 'a t list -> 'a list t

(** Extends [or_else] for a list of parsers *)
val choice : 'a t array -> 'a t

(** Match a parser zero or more times. *)
val many : 'a t -> 'a list t

(** Match a parser one or more times. *)
val many1 : 'a t -> 'a list t

(** Match a parser zero or one time. *)
val opt : 'a t -> 'a option t

(** Consume and drop the first parser and consume and keep the second one. *)
val drop_and : 'a t -> 'b t -> 'b t
val (!>) : 'a t -> 'b t -> 'b t

(** Consume and keep the first parser and consume and drop the seconds one. *)
val and_drop : 'a t -> 'b t -> 'a t
val (>!) : 'a t -> 'b t -> 'a t

(** Drops the first and last parsers and keeps the one in the middle. *)
val between : 'a t -> 'b t -> 'c t -> 'b t

(** Parses one or more occurences of a parser with a separator. *)
val sepby1 : 'a t -> 'b t -> 'a list t

(** Parses zero or more occurences of a parser with a separator. *)
val sepby : 'a t -> 'b t -> 'a list t

val satisfy : (string -> bool) -> label -> string t

val any : string array -> string t

val pchar : character -> character t

val pstring : string -> string t

val digit_char : character t
val whitespace_char : character t
val spaces : string list t
val spaces1 : string list t
val dquote_char : character t

val pint : int t
val pfloat : float t
