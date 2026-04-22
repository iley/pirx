type t = { file : string; line : int; col : int }
[@@deriving sexp, compare, equal]

val to_string : t -> string
