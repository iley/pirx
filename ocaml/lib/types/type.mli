type t =
  | Int
  | Int8
  | Int64
  | Bool
  | String
  | Void
  | Undefined
  | Pointer of t
  | Slice of t
[@@deriving sexp, equal]

val to_string : t -> string
