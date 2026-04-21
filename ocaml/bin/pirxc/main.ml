(* pirxc — OCaml port of the Pirx compiler.
   Milestone M0: flag parsing + hardcoded aarch64-darwin assembly for the empty
   program. Input files are not read; this exists to prove the build/install/
   test plumbing end-to-end against testrunner. *)

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

let derive_output_name = function
  | [one] -> strip_pirx_ext one ^ ".s"
  | _ ->
    die "When more than one input file name is provided, you must specify an output file name via -o"

let write_output out_spec content =
  if out_spec = "-" then print_string content
  else
    let oc = open_out out_spec in
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () -> output_string oc content)

let () =
  let output = ref "" in
  let no_opt = ref false in
  let target = ref default_target in
  let inputs = ref [] in
  let specs = [
    ("-o",  Arg.Set_string output, " output file name");
    ("-O0", Arg.Set no_opt,        " don't optimize");
    ("-t",  Arg.Set_string target, " target architecture");
  ] in
  Arg.parse specs (fun f -> inputs := f :: !inputs) usage;
  let inputs = List.rev !inputs in
  if inputs = [] then begin
    prerr_endline usage;
    exit 1
  end;
  ignore !no_opt;
  ignore !target;
  let out_spec =
    if !output = "" then derive_output_name inputs else !output
  in
  write_output out_spec hardcoded_asm
