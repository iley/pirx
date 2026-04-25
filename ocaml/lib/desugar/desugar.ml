open Core
open Pirx_ast.Ast
module Type = Pirx_types.Type

let run program =
  let rec desugar_expr (e : expr) : expr =
    match e.kind with
    | E_string_lit s ->
      let len = String.length s in
      let int_lit : expr =
        { kind = E_int_lit (Int64.of_int len); loc = e.loc; typ = Type.Int }
      in
      { kind = E_call { name = "PirxString"; args = [int_lit; e] };
        loc = e.loc;
        typ = Type.String }
    | E_int_lit _ | E_ident _ -> e
    | E_call { name; args } ->
      { e with kind = E_call { name; args = List.map args ~f:desugar_expr } }
  in
  let desugar_stmt = function
    | S_var_decl d ->
      S_var_decl { d with init = Option.map d.init ~f:desugar_expr }
    | S_assign a ->
      S_assign { a with target = desugar_expr a.target;
                        value  = desugar_expr a.value }
    | S_expr e -> S_expr (desugar_expr e)
    | S_return r ->
      S_return { r with value = Option.map r.value ~f:desugar_expr }
  in
  let desugar_func (f : func) =
    { f with body = List.map f.body ~f:desugar_stmt }
  in
  { functions = List.map program.functions ~f:desugar_func }
