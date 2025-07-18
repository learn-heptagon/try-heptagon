open Obc
open Obc_interp

exception InterpreterError of string
let () =
  Printexc.register_printer
    (function
      | InterpreterError s -> Some (Printf.sprintf "Interpreter error: %s" s)
      | _ -> None)

(* let interpreter_error (msg : Errors.errcode list) = *)
(*   Format.fprintf Format.str_formatter "%a" Driveraux.print_error msg; *)
(*   raise (InterpreterError (Format.flush_str_formatter ())) *)

(* let ident_of_str s = Ident.str_to_pos @@ Camlcoq.coqstring_of_camlstring @@ s *)
(* let str_of_ident x = Camlcoq.camlstring_of_coqstring @@ Ident.pos_to_str @@ x *)

(* Interpreting Obc *)
module ObcInterpreter(P : sig
    val prog : Obc.program
    val classname : string
  end) : Simul.Interpreter = struct
  let mem = ref Obc_interp.init_memory

  let reset () =
    mem := Obc_interp.reset P.prog P.classname

  let step ins =
    let (outs, nmem) = Obc_interp.step P.prog P.classname ins !mem in
    mem := nmem;
    outs
end

(* Executing JS *)

open Js_of_ocaml
open Js_obc_conversion

let js_eval (code : string) =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "eval") [| Js.Unsafe.inject (Js.string code) |]

module JsInterpreter(P : sig
    val prog : Obc.program
    val classname : string
  end) : Simul.Interpreter = struct

  let mem =
    let js_prog = Obc2javascript.program P.prog in
    Javascript_printer.program Format.str_formatter js_prog;
    let js_code = Format.flush_str_formatter () in
    print_endline js_code;
    let js_code = js_code ^ "new "^(String.capitalize_ascii P.classname)^"()" in
    (* let js_code = String.concat "\\\n" (String.split_on_char '\n' js_code) in *)
    js_eval js_code

  let reset () =
    Js.Unsafe.meth_call mem "reset" [||]

  let step ins =
    let found_class = find_class P.prog P.classname in
    let found_method = find_method found_class Mstep in
    let output_types = List.map (fun v -> v.v_type) found_method.m_outputs in

    let js_result = Js.Unsafe.meth_call mem "step" (Array.of_list (List.map js_of_obc ins)) in

    match output_types with
      | [] -> []
      | [ty_elem] -> [obc_of_js ty_elem js_result]
      | ty_list ->
        let js_arr = Js.Unsafe.coerce js_result in
        List.mapi (fun i ty ->
          let js_val = Js.array_get js_arr i in
          obc_of_js ty (Js.Optdef.get js_val (fun _ -> failwith "Should not happen"))
        ) output_types
end

module DefaultInterpreter = JsInterpreter

(* Interpreter related functions *)

type interp_type =
  | Chronogram (* Simply fill a chronogram *)
  | Simulator of (module Simul.Simulator)

let old_node = ref None

let rec choose_default names old =
  match names, old with
  | [], _ -> raise (InterpreterError "No available node")
  | [n], _ -> n
  | hd::tl, Some old when old = hd -> hd
  | _::tl, _ -> choose_default tl old

let ( let* ) x f =
  match x with
  | Some x -> f x
  | None -> ()

let stop_fun = ref (fun _ -> ())

let saved_st = ref None

let load_interp (panelid : string) (prog: Obc.program) int =
  !stop_fun ();
  let editorid = panelid^"-editor" in
  (try Page.clear_div editorid with _ -> ());
  match int with
  | Chronogram ->
    let names = List.filter_map (function Pclass cl -> Some cl.cd_name.name | _ -> None) prog.p_desc in
    Page.create_select editorid names (choose_default names !old_node)
      (fun cname ->
        old_node := Some cname;
        let met = Obc_interp.find_method (Obc_interp.find_class prog cname) Mstep in

        let open DefaultInterpreter(struct let prog = prog let classname = cname end) in

        let open Chronogram in

        let interface_matches inputs outputs st =
          List.map (fun info -> (info.row.var_name, info.row.var_type)) st.inputs = List.map (fun vd -> (Idents.source_name vd.v_ident, vd.v_type)) inputs
          && List.map (fun info -> (info.var_name, info.var_type)) st.outputs = List.map (fun vd -> (Idents.source_name vd.v_ident, vd.v_type)) outputs
        in

        let st =
          match !saved_st with
          | Some st when interface_matches met.m_inputs met.m_outputs st -> st
          | _ ->
            let st =
              Chronogram.make
                (List.map (fun vd -> (Idents.source_name vd.v_ident, vd.v_type)) met.m_inputs)
                (List.map (fun vd -> (Idents.source_name vd.v_ident, vd.v_type)) met.m_outputs)
            in
            saved_st := Some st;
            st
        in

        Chronogram.recompute_outputs step st;

        Page.show_chronogram editorid st reset step
      )
  | Simulator simul ->
    let (module Simul) = simul in
    stop_fun := Simul.init editorid

let interpreter_of_example s p =
  (* Obc_printer.print_prog Format.std_formatter p; *)
  match s with
  (* | "full-adder.lus" -> *)
  (*   Simulator (module Simul.TruthTable) *) (* TODO *)
  | "filters.lus" | "fifo-filters.lus" ->
    Simulator (module Simul.FilterSimul(
                          DefaultInterpreter(struct
                              let prog = p
                              let classname = "system"
      end)))
  | "stopwatch.lus" ->
    Simulator (module Simul.StopwatchSimul(
                          DefaultInterpreter(struct
                              let prog = p
                              let classname = "stopwatch"
                            end)
      ))
  | "fifo-audio.lus" ->
    Simulator (module Simul.AudioFilterSimul(
                          DefaultInterpreter(struct
                              let prog = p
                              let classname = "main"
                            end)
      ))
  (* | "stepper.lus" -> Simulator (module Stepper_simul.StepperSimul( *)
  (*       DefaultInterpreter(struct *)
  (*         let prog = p *)
  (*         let classname = "motor" *)
  (*       end))) *)
  (* | "porte_telecabine.lus" -> Simulator (module Porte_telecabine_simul.Simul( *)
  (*       DefaultInterpreter(struct *)
  (*         let prog = p *)
  (*         let classname = "control_porte" *)
  (*       end) *)
  (*     )) *)
  | _ -> Chronogram
