module String : sig
  type t = string

  val is_empty : t -> bool

  val to_array : t -> t array

  val to_list : t -> t list

  val from_array : t array -> t

  val from_list : t list -> t
end

type error = string
type stream = string
type character = string
type 'a t = Parser of (stream -> (('a * stream), error) Belt.Result.t)

val run : 'a t -> stream -> (('a * stream), error) Belt.Result.t

val bind : 'a t -> ('a -> 'b t) -> 'b t
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

val map : 'a t -> ('a -> 'b) -> 'b t
val (|>>) : 'a t -> ('a -> 'b) -> 'b t

val return : 'a -> 'a t

val apply : ('a -> 'b) t -> 'a t -> 'b t
val (<*>) : ('a -> 'b) t -> 'a t -> 'b t

val and_then : 'a t -> 'b t -> ('a * 'b) t
val (>>) : 'a t -> 'b t -> ('a * 'b) t

val or_else : 'a t -> 'a t -> 'a t
val (<|>) : 'a t -> 'a t -> 'a t

val sequence : 'a t list -> 'a list t

val choice : 'a t array -> 'a t

val any : string array -> string t

val parse_zero_or_more : 'a t -> stream -> 'a list * stream

(** Match a parser zero or more times. *)
val many : 'a t -> 'a list t

(** Match a parser one or more times. *)
val many1 : 'a t -> 'a list t

(** Match a parser zero or one time. *)
val opt : 'a t -> 'a option t

val drop_and : 'a t -> 'b t -> 'b t
val (!>) : 'a t -> 'b t -> 'b t

val and_drop : 'a t -> 'b t -> 'a t
val (>!) : 'a t -> 'b t -> 'a t

val between : 'a t -> 'b t -> 'c t -> 'b t

val sepby1 : 'a t -> 'b t -> 'a list t

val sepby : 'a t -> 'b t -> 'a list t

val pchar : character -> character t

val pstring : string -> string t

val pdigit : character t

val pint : int t
