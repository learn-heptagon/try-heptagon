(** Having all functions that call Heptagon compiler functions, but none that manipulate the DOM **)

open Compiler_utils
open Compiler_options

(** The next three functions are transferred from tryhept.ml **)

(* [modname] is the module name, [source_f] is the source file *)
let parse_program modname source =
  let lexbuf = Lexing.from_string source in

  (* Activates passes according to the backend used *)
  Mls2seq.load_conf ();
  (* Process the [lexbuf] to an Heptagon AST *)
  Hept_parser_scoper.parse_program modname lexbuf stdout

(* Check the program *)
let check_program p log_c =
  let pp p = if !verbose then Hept_printer.print log_c p in

  let p = silent_pass "Statefulness check" true Stateful.program p in
  let p = silent_pass "Unsafe check" true Unsafe.program p in
  let p = pass "Typing" true Typing.program p pp in
  let p = pass "Linear Typing" !do_linear_typing Linear_typing.program p pp in
  let p = pass "Inlining" true Inline.program p pp in
  let p = pass "Contracts" true Contracts.program p pp in
  let p = silent_pass "Causality check" !causality Causality.program p in
  let _ = silent_pass "Initialization check" !init Initialization.program p in
  ()

(* [modname] is the module name, [p] is the heptagon program *)
let compile_program modname p =
  let minils_c = open_out (modname^".mls") in
  let log_c = stdout in

  let close_all () =
    close_out minils_c
  in

  (* Process the Heptagon AST *)
  let p = Hept_compiler.compile_program p log_c in
  (* Compile Heptagon to MiniLS *)
  let p = Hept2mls.program p in
  (* Output the .mls *)
  Mls_printer.print minils_c p;
  (* Process the MiniLS AST *)
  let p = Mls_compiler.compile_program p log_c in
  (* Compile MiniLS *)
  let p = Mls2obc.program p in
  let p = Obc_compiler.compile_program log_c p in
  Mls2seq.write_obc_file p;
  close_all (); p

let build_program lexbuf =
  let ast = Hept_parser_scoper.parse Hept_parser.inline_exp lexbuf in
  let location = ast.e_loc in
  let var_name = "y" in

  let var_dec : Hept_parsetree.var_dec = {
    v_name = var_name;
    v_type = Tid (Q (Names.qualname_of_string "id"));
    v_linearity = Linearity.Ltop;
    v_clock = None;
    v_last = Var;
    v_loc = location;
  } in

  let eq : Hept_parsetree.eq = {
    eq_desc = Eeq (Evarpat var_name, Linearity.Lno_init, ast);
    eq_loc = location;
  } in

  let block : Hept_parsetree.block = {
    b_local = [];
    b_equs = [eq];
    b_loc = location;
  } in

  let dec_name = "main" in

  let node_dec : Hept_parsetree.node_dec = {
    n_name = dec_name;
    n_stateful = false;
    n_unsafe = false;
    n_input = [];
    n_output = [var_dec];
    n_contract = None;
    n_block = block;
    n_loc = location;
    n_params = [];
    n_constraints = [];
  } in

  let parsetree_program : Hept_parsetree.program = {
    p_modname = dec_name;
    p_opened = [];
    p_desc = [Pnode node_dec];
  } in

  let program = Hept_scoping.translate_program parsetree_program in

  Some program
