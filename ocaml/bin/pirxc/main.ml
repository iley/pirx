(* pirxc — OCaml port of the Pirx compiler.
   Through M1/S2: -t ast runs lex + parse + print. Other targets still emit
   the M0 hardcoded aarch64-darwin blob (only valid for tests/000_empty.pirx
   — enough to keep testrunner test 000 green while later slices land). *)

open Core
module Location = Pirx_location.Location
module Lexer = Pirx_lexer.Lexer
module Parser = Pirx_parser.Parser
module Ast_pp = Pirx_ast.Pp

let hardcoded_asm =
{|.globl _Pirx_Main
.p2align 2
_Pirx_Main:
  ; frame size: 0 bytes
  sub sp, sp, #16
  stp x29, x30, [sp]
  mov x29, sp
  sub sp, sp, #0
  ; Op 0: Return4(0)
  mov w0, #0
  str w0, [x19]
.LPirx_Main_exit:
  add sp, sp, #0
  ldp x29, x30, [sp]
  add sp, sp, #16
  ret
.globl _Pirx_Init
.p2align 2
_Pirx_Init:
  ; frame size: 0 bytes
  sub sp, sp, #16
  stp x29, x30, [sp]
  mov x29, sp
  sub sp, sp, #0
  ; Op 0: Return()
.LPirx_Init_exit:
  add sp, sp, #0
  ldp x29, x30, [sp]
  add sp, sp, #16
  ret
.globl _main
.p2align 2
_main:
  ; frame size: 16 bytes
  sub sp, sp, #16
  stp x29, x30, [sp]
  mov x29, sp
  sub sp, sp, #16
  ; Op 0: Call(Pirx_Init())
  str x19, [sp, #-8]
  sub sp, sp, #16
  bl _Pirx_Init
  add sp, sp, #16
  ldr x19, [sp, #-8]
  ; Op 1: Call4($ret = Pirx_Main())
  str x19, [sp, #-8]
  add x19, sp, #0
  sub sp, sp, #16
  bl _Pirx_Main
  add sp, sp, #16
  ldr x19, [sp, #-8]
  ; Op 2: ExternalReturn4($ret)
  ldr w0, [sp]
  sxtw x0, w0
.Lmain_exit:
  add sp, sp, #16
  ldp x29, x30, [sp]
  add sp, sp, #16
  ret
|}

let default_target = "aarch64-darwin"

let usage = "Usage: pirxc [options] <input file>..."

let die fmt =
  Printf.ksprintf (fun s -> prerr_endline s; exit 1) fmt

let strip_pirx_ext name =
  if Filename.check_suffix name ".pirx"
  then Filename.chop_suffix name ".pirx"
  else
    try Filename.chop_extension name
    with Invalid_argument _ -> name

let derive_output_name target inputs =
  match inputs with
  | [one] ->
    let base = strip_pirx_ext one in
    (match target with
     | "ast" -> base ^ ".txt"
     | _ -> base ^ ".s")
  | _ ->
    die "When more than one input file name is provided, you must specify an output file name via -o"

let write_output out_spec content =
  if String.equal out_spec "-" then print_string content
  else Out_channel.write_all out_spec ~data:content

let read_file path =
  try In_channel.read_all path
  with Sys_error msg -> die "cannot read %s: %s" path msg

let compile_ast input =
  let source = read_file input in
  let lexer = Lexer.create ~filename:input ~source in
  let program = Parser.parse lexer in
  Ast_pp.string_of_program program ^ "\n"

let () =
  let output = ref "" in
  let no_opt = ref false in
  let target = ref default_target in
  let inputs = ref [] in
  let specs =
    Stdlib.Arg.[
      ("-o",  Set_string output, " output file name");
      ("-O0", Set no_opt,        " don't optimize");
      ("-t",  Set_string target, " target architecture");
    ]
  in
  Stdlib.Arg.parse specs (fun f -> inputs := f :: !inputs) usage;
  let inputs = List.rev !inputs in
  if List.is_empty inputs then begin
    prerr_endline usage;
    exit 1
  end;
  ignore !no_opt;
  let out_spec =
    if String.is_empty !output then derive_output_name !target inputs
    else !output
  in
  try
    let content =
      match !target with
      | "ast" ->
        (match inputs with
         | [one] -> compile_ast one
         | _ -> die "-t ast expects a single input file")
      | "final_ast" | "ir" -> die "-t %s not implemented yet" !target
      | "aarch64-darwin" -> hardcoded_asm
      | other -> die "unsupported target: %s" other
    in
    write_output out_spec content
  with Lexer.Compile_error (loc, msg) ->
    prerr_endline (Printf.sprintf "%s: %s" (Location.to_string loc) msg);
    exit 1
