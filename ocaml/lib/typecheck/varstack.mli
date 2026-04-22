module Type = Pirx_types.Type

type t

val create : unit -> t
val push_scope : t -> unit
val pop_scope : t -> unit

(* Declare [name] in the innermost scope. Returns [`Duplicate_in_scope] if
   [name] is already bound in that same scope; outer-scope shadowing is fine. *)
val declare : t -> string -> Type.t -> [ `Ok | `Duplicate_in_scope ]

(* Innermost-first lookup across all open scopes. *)
val lookup : t -> string -> Type.t option
