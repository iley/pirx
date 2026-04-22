module Location = Pirx_location.Location

type entry = { loc : Location.t; msg : string }

type t

val create : unit -> t
val push : t -> Location.t -> string -> unit
val pushf : t -> Location.t -> ('a, unit, string, unit) format4 -> 'a
val entries : t -> entry list
val is_empty : t -> bool
