module String : sig
  type t = string

  val is_empty : t -> bool

  val to_array : t -> t array

  val to_list : t -> t list

  val from_array : t array -> t

  val from_list : t list -> t

  val is_digit : t -> bool

  val is_whitespace : t -> bool
end

type label = string

(** A parsing error. *)
type error = string

(** A stream of characters to be parsed. *)
type stream = string

type 'a parser = stream -> ('a * stream, label * error) Belt.Result.t

(** A character. Not using [char] because they are not utf-8 aware. *)
type character = string

(** A parser is a function of an input stream and results in a pair of the
    parsed fraction and the remaining stream. If it fails, returns an [Error]. *)
type 'a t = Parser of { parser : 'a parser; label : label }

val set_label : 'a t -> label -> 'a t
val (<?>) : 'a t -> label -> 'a t

(** Consumes the given input with the given parser. *)
val run : 'a t -> stream -> ('a * stream, label * error) Belt.Result.t

(** Chains the result of a parser to another parser. *)
val bind : 'a t -> ('a -> 'b t) -> 'b t
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

(** Transforms the result of a parser. *)
val map : 'a t -> ('a -> 'b) -> 'b t
val (|>>) : 'a t -> ('a -> 'b) -> 'b t

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
val dquote_char : character t

val pint : int t
