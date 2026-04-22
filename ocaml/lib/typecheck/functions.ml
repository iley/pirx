open Core
module Ast = Pirx_ast.Ast

type t = { table : Proto.t String.Table.t }

let proto_of_func (f : Ast.func) : Proto.t =
  {
    name = f.name;
    external_name = None;
    args = f.args;
    ret_type = f.ret_type;
    variadic = false;
    external_ = f.external_;
  }

(* Last-write-wins, matching Go (typechecker.go Check: `c.declaredFuncs[p.Name] = p`).
   User functions silently shadow builtins of the same name; duplicate user
   functions silently overwrite earlier ones. No diagnostic at this stage —
   the Go checker doesn't emit one either. [diag] is still threaded so later
   milestones can start reporting at this point without a signature change. *)
let build (program : Ast.program) (_diag : Diag.t) : t =
  let table = String.Table.create () in
  List.iter Builtins.protos ~f:(fun p ->
    Hashtbl.set table ~key:p.name ~data:p);
  List.iter program.functions ~f:(fun f ->
    Hashtbl.set table ~key:f.name ~data:(proto_of_func f));
  { table }

let lookup t name = Hashtbl.find t.table name
