open Core
open Ir
module Ast = Pirx_ast.Ast
module Type = Pirx_types.Type
module Functions = Pirx_typecheck.Functions
module Diag = Pirx_typecheck.Diag

let type_size = function
  | Type.Int -> 4
  | Type.Int8 -> 1
  | Type.Int64 -> 8
  | Type.Bool -> 4
  | Type.String -> 16
  | Type.Void -> 0
  | Type.Undefined -> 0
  | Type.Pointer _ -> 8
  | Type.Slice _ -> 16

type state = {
  funcs : Functions.t;
  mutable next_temp : int;
  mutable is_external : bool;
}

let create_state funcs = {
  funcs;
  next_temp = 1;
  is_external = false;
}

let alloc_temp state =
  let idx = state.next_temp in
  state.next_temp <- idx + 1;
  Printf.sprintf "$%d" idx

let rec generate_expr state (e : Ast.expr) : op list * arg * int =
  match e.kind with
  | E_int_lit n ->
    ([], Int_lit n, 4)
  | E_string_lit s ->
    ([], String_lit s, 8)
  | E_ident { name } ->
    let size = type_size e.typ in
    ([], Var name, size)
  | E_call { name = func_name; args = arg_exprs } ->
    generate_call state func_name arg_exprs e.typ

and generate_call state func_name arg_exprs ret_type =
  let ops, call_args =
    List.fold_map arg_exprs ~init:[] ~f:(fun acc_ops arg_expr ->
      let arg_ops, arg_arg, arg_size = generate_expr state arg_expr in
      let call_arg = { arg = arg_arg; size = arg_size; is_float = false } in
      (acc_ops @ arg_ops, call_arg))
  in
  match Functions.lookup state.funcs func_name with
  | None ->
    failwith (Printf.sprintf "unknown function %s" func_name)
  | Some proto ->
    let result_size = type_size ret_type in
    let temp, result =
      if Type.equal proto.ret_type Type.Void then
        (None, None)
      else
        let t = alloc_temp state in
        (Some t, Some t)
    in
    let call_op =
      if proto.external_ then
        let ext_name = Option.value proto.external_name ~default:func_name in
        ExternalCall {
          result = result;
          func = ext_name;
          args = call_args;
          named_args = List.length proto.args;
          size = result_size;
        }
      else
        Call {
          result = result;
          func = func_name;
          args = call_args;
          size = result_size;
        }
    in
    let result_arg = Option.value_map temp ~default:Zero ~f:(fun t -> Var t) in
    (ops @ [call_op], result_arg, result_size)

let generate_stmt state (s : Ast.stmt) : op list =
  match s with
  | S_var_decl { name; typ; init; _ } ->
    let declared_ty =
      match typ with
      | Some t -> t
      | None ->
        (match init with
         | Some e -> e.typ
         | None -> Type.Undefined)
    in
    let size = type_size declared_ty in
    (match init with
     | None ->
       [Assign { target = name; value = Zero; size }]
     | Some e ->
       let ops, arg, _ = generate_expr state e in
       ops @ [Assign { target = name; value = arg; size }])
  | S_assign { target; value; _ } ->
    (match target.kind with
     | E_ident { name } ->
       let ops, arg, size = generate_expr state value in
       ops @ [Assign { target = name; value = arg; size }]
     | _ ->
       failwith "invalid assignment target")
  | S_expr e ->
    let ops, _, _ = generate_expr state e in
    ops
  | S_return { value; _ } ->
    (match value with
     | None ->
       if state.is_external then
         [ExternalReturn { value = None; size = 0 }]
       else
         [Return { value = None; size = 0 }]
     | Some e ->
       let ops, arg, size = generate_expr state e in
       if state.is_external then
         ops @ [ExternalReturn { value = Some arg; size }]
       else
         ops @ [Return { value = Some arg; size }])

let generate_func state (f : Ast.func) : Ir.func =
  let name = if String.equal f.name "main" then "Pirx_Main" else f.name in
  state.next_temp <- 1;
  state.is_external <- f.external_;
  let args = List.map f.args ~f:fst in
  let arg_sizes = List.map f.args ~f:(fun (_, ty) -> type_size ty) in
  let ops = List.concat_map f.body ~f:(generate_stmt state) in
  let ops =
    let has_return =
      match List.last ops with
      | Some (Return _) | Some (ExternalReturn _) -> true
      | _ -> false
    in
    if has_return then ops
    else if state.is_external then
      ops @ [ExternalReturn { value = None; size = 0 }]
    else
      ops @ [Return { value = None; size = 0 }]
  in
  { name; args; arg_sizes; ops }

let generate_init () : func =
  { name = "Pirx_Init"; args = []; arg_sizes = []; ops = [Return { value = None; size = 0 }] }

let generate_main () : func =
  let ops = [
    Call { result = None; func = "Pirx_Init"; args = []; size = 0 };
    Call { result = Some "$ret"; func = "Pirx_Main"; args = []; size = 4 };
    ExternalReturn { value = Some (Var "$ret"); size = 4 };
  ] in
  { name = "main"; args = []; arg_sizes = []; ops }

let generate (program : Ast.program) : program =
  let funcs = Functions.build program (Diag.create ()) in
  let state = create_state funcs in
  let ir_funcs = List.map program.functions ~f:(generate_func state) in
  { functions = ir_funcs @ [generate_init (); generate_main ()] }
