open Core
module Type = Pirx_types.Type
module Ast = Pirx_ast.Ast

type ctx = {
  diag : Diag.t;
  funcs : Functions.t;
  vars : Varstack.t;
  func_ret_type : Type.t;
  func_name : string;
}

(* Two types are "compatible" if either side is [Undefined] (meaning a prior
   error suppressed inference) or they are structurally equal. Centralizing
   the rule keeps error cascades out of every check site. *)
let compatible (a : Type.t) (b : Type.t) : bool =
  match a, b with
  | Undefined, _ | _, Undefined -> true
  | _ -> Type.equal a b

let rec check_expr (ctx : ctx) (e : Ast.expr) : unit =
  let t =
    match e.kind with
    | E_int_lit _ -> Type.Int
    | E_string_lit _ -> Type.String
    | E_ident name ->
      (match Varstack.lookup ctx.vars name with
       | Some t -> t
       | None ->
         Diag.pushf ctx.diag e.loc "undeclared identifier '%s'" name;
         Type.Undefined)
    | E_call { name; args } ->
      List.iter args ~f:(check_expr ctx);
      check_call ctx e.loc ~name ~args
  in
  e.typ <- t

and check_call ctx loc ~name ~args : Type.t =
  match Functions.lookup ctx.funcs name with
  | None ->
    Diag.pushf ctx.diag loc "undeclared function '%s'" name;
    Type.Undefined
  | Some proto ->
    let expected = List.length proto.args in
    let actual = List.length args in
    let arity_ok =
      if proto.variadic then actual >= expected
      else actual = expected
    in
    if not arity_ok then begin
      if proto.variadic then
        Diag.pushf ctx.diag loc
          "function '%s' expects at least %d argument(s), got %d"
          name expected actual
      else
        Diag.pushf ctx.diag loc
          "function '%s' expects %d argument(s), got %d"
          name expected actual
    end;
    (* Type-check the named-arg portion; variadic tail is untyped by design. *)
    List.iteri args ~f:(fun i arg ->
      if i < expected then
        let _, expected_ty = List.nth_exn proto.args i in
        if not (compatible arg.typ expected_ty) then
          Diag.pushf ctx.diag arg.loc
            "argument %d of '%s' has type %s, expected %s"
            (i + 1) name (Type.to_string arg.typ) (Type.to_string expected_ty));
    proto.ret_type

let is_lvalue (e : Ast.expr) : bool =
  match e.kind with
  | E_ident _ -> true
  | _ -> false

let rec check_stmt (ctx : ctx) (s : Ast.stmt) : unit =
  match s with
  | S_var_decl { loc; name; typ; init } -> check_var_decl ctx ~loc ~name ~typ ~init
  | S_assign { target; value; _ } -> check_assign ctx ~target ~value
  | S_expr e -> check_expr ctx e
  | S_return { loc; value } -> check_return ctx ~loc ~value

and check_var_decl ctx ~loc ~name ~typ ~init =
  let declared_ty =
    match typ, init with
    | Some t, Some e ->
      check_expr ctx e;
      if not (compatible e.typ t) then
        Diag.pushf ctx.diag e.loc
          "initializer has type %s, expected %s"
          (Type.to_string e.typ) (Type.to_string t);
      t
    | Some t, None -> t
    | None, Some e ->
      check_expr ctx e;
      e.typ
    | None, None ->
      Diag.pushf ctx.diag loc
        "variable '%s' needs either a type annotation or an initializer" name;
      Type.Undefined
  in
  match Varstack.declare ctx.vars name declared_ty with
  | `Ok -> ()
  | `Duplicate_in_scope ->
    Diag.pushf ctx.diag loc "variable '%s' is already declared in this scope" name

and check_assign ctx ~target ~value =
  check_expr ctx target;
  check_expr ctx value;
  if not (is_lvalue target) then
    Diag.push ctx.diag target.loc "assignment target is not an lvalue";
  if not (compatible target.typ value.typ) then
    Diag.pushf ctx.diag value.loc
      "cannot assign value of type %s to target of type %s"
      (Type.to_string value.typ) (Type.to_string target.typ)

and check_return ctx ~loc ~value =
  match value, ctx.func_ret_type with
  | None, Type.Void -> ()
  | None, _ ->
    Diag.pushf ctx.diag loc
      "function '%s' must return a value of type %s"
      ctx.func_name (Type.to_string ctx.func_ret_type)
  | Some e, Type.Void ->
    check_expr ctx e;
    Diag.pushf ctx.diag e.loc
      "function '%s' returns void, cannot return a value" ctx.func_name
  | Some e, expected ->
    check_expr ctx e;
    if not (compatible e.typ expected) then
      Diag.pushf ctx.diag e.loc
        "returned value has type %s, expected %s"
        (Type.to_string e.typ) (Type.to_string expected)

let check_func (diag : Diag.t) (funcs : Functions.t) (f : Ast.func) : unit =
  if f.external_ then ()
  else begin
    let vars = Varstack.create () in
    (* Params in the function scope; body opens its own scope so a [var]
       with the same name shadows the param instead of colliding.
       Mirrors Go (typechecker.go checkFunction opens a scope for params;
       checkBlock opens another for the body). *)
    List.iter f.args ~f:(fun (name, ty) ->
      match Varstack.declare vars name ty with
      | `Ok -> ()
      | `Duplicate_in_scope ->
        Diag.pushf diag f.loc "duplicate function argument: %s" name);
    let ctx =
      { diag; funcs; vars;
        func_ret_type = f.ret_type; func_name = f.name }
    in
    Varstack.push_scope vars;
    List.iter f.body ~f:(check_stmt ctx);
    Varstack.pop_scope vars
  end

let check (program : Ast.program) : Ast.program * Diag.t =
  let diag = Diag.create () in
  let funcs = Functions.build program diag in
  List.iter program.functions ~f:(check_func diag funcs);
  program, diag
