module Type = Pirx_types.Type

type binding = {
  unique_name : string;
  typ : Type.t;
}

type t

val create : unit -> t
val push_scope : t -> unit
val pop_scope : t -> unit

(* Declare [name] in the innermost scope with the given type. Returns
   [`Ok unique_name] on success, where [unique_name] is a globally-unique
   identifier for this binding (e.g. [x] for the first [x] in a function,
   [x#1] for the second, etc.). Returns [`Duplicate_in_scope] if [name] is
   already bound in that same scope; outer-scope shadowing is fine. *)
val declare : t -> string -> Type.t -> [ `Ok of string | `Duplicate_in_scope ]

(* Innermost-first lookup across all open scopes. *)
val lookup : t -> string -> binding option
