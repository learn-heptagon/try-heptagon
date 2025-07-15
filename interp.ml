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

module DefaultInterpreter = ObcInterpreter

(* Interpreter related functions *)

type interp_type =
  | Chronogram (* Simply fill a chronogram *)
  | Simulator of (module Simul.Simulator)

let old_node = ref None

let rec choose_default names old =
  match names, old with
  | [], _ -> invalid_arg "choose_default"
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
