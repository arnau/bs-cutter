type t = string

val length : t -> int
val get : t -> int -> t
val concat : t -> t -> t
val concatMany : t array -> t -> t
val endsWith : t -> t -> bool
val endsWithFrom : t -> int -> t -> bool
val includes : t -> t -> bool
val includesFrom : t -> int -> t -> bool
val indexOf : t -> t -> int
val indexOfFrom : t -> int -> t -> int
val lastIndexOf : t -> t -> int
val lastIndexOfFrom : t -> int -> t -> int
val localeCompare : t -> t -> float
val match_ : Js.Re.t -> t -> t array option
val repeat : int -> t -> t
val replace : t -> t -> t -> t
val slice : from:int -> to_:int -> t -> t
val sliceToEnd : from:int -> t -> t
val split : t -> t -> t array
val splitAtMost : t -> limit:int -> t -> t array
val splitByRe : Js_re.t -> t -> t option array
val startsWith : t -> t -> bool
val substr : from:int -> t -> t
val substrAtMost : from:int -> length:int -> t -> t
val toLowerCase : t -> t
val toUpperCase : t -> t
val trim : t -> t

val is_empty : t -> bool

val to_array : t -> t array

val to_list : t -> t list

val from_array : t array -> t

val from_list : t list -> t

(** Should be in Char module *)
val is_digit : t -> bool

val is_whitespace : t -> bool





