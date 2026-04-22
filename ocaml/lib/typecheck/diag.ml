open Core
module Location = Pirx_location.Location

type entry = { loc : Location.t; msg : string }

type t = { mutable rev_entries : entry list }

let create () = { rev_entries = [] }

let push t loc msg =
  t.rev_entries <- { loc; msg } :: t.rev_entries

let pushf t loc fmt =
  Printf.ksprintf (push t loc) fmt

let entries t = List.rev t.rev_entries

let is_empty t = List.is_empty t.rev_entries
