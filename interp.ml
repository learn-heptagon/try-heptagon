open Obc
open Obc_interp

(* Access interpreter functions from Heptagon *)
module type Interpreter = sig
  (** Interpreter syntax *)
  type syn

  (** Interpreter state *)
  type state

  (** Init/reinit the interpreter from the AST the program *)
  val reset : syn -> string -> state

  (** Call the given node, take a step *)
  val step : syn -> string -> state -> value option list -> (value option list * state)
end

exception InterpreterError of string
exception ParseInputError of string
let () =
  Printexc.register_printer
    (function
      | InterpreterError s -> Some (Printf.sprintf "Interpreter error: %s" s)
      | ParseInputError v -> Some (Printf.sprintf "Couldn't parse input %s" v)
      | _ -> None)

(* let interpreter_error (msg : Errors.errcode list) = *)
(*   Format.fprintf Format.str_formatter "%a" Driveraux.print_error msg; *)
(*   raise (InterpreterError (Format.flush_str_formatter ())) *)

(* let ident_of_str s = Ident.str_to_pos @@ Camlcoq.coqstring_of_camlstring @@ s *)
(* let str_of_ident x = Camlcoq.camlstring_of_coqstring @@ Ident.pos_to_str @@ x *)

(* Interpreting Obc *)
module ObcInterpreter : Interpreter with type syn = Obc.program = struct
  type syn = Obc.program
  type state = Obc_interp.memory

  let reset p cname = Obc_interp.reset p cname

  let step p cname me ins = Obc_interp.step p cname ins me
end

module ObcSimulInterpreter(P : sig
    val prog : Obc.program
    val classname : string
  end) : Simul.Interpreter = struct
  let mem = ref (ObcInterpreter.reset P.prog P.classname)

  let reset () =
    mem := ObcInterpreter.reset P.prog P.classname

  let step ins =
    let (outs, nmem) = ObcInterpreter.step P.prog P.classname !mem ins in
    mem := nmem;
    outs
end

(* Interpreter related functions *)

type interp_type =
  | Chronogram (* Simply fill a chronogram *)
  | Simulator of (module Simul.Simulator)

(* let step_fun = ref (fun () -> ()) *)

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

let parse_input typ s =
  try
    if s = "." then None
    else
      let open Types in
      Some (match typ with
            | Tid { name = "bool" } -> Vbool (s = "true")
            | Tid { name = "int" } -> Vint (int_of_string s)
            | Tid { name = "real" } | Tid { name = "float" } -> Vfloat (float_of_string s)
            | _ -> failwith "TODO")
  with _ -> raise (ParseInputError s)

let string_of_value = function
  | Vbool b -> if b then "true" else "false"
  | Vint i -> string_of_int i
  | Vfloat f -> string_of_float f

let string_of_svalue = function
  | None -> "."
  | Some v -> string_of_value v

let stop_fun = ref (fun _ -> ())

let is_boolean_type =
  Types.(function
         | Tid { name = "bool" } -> true
         | _ -> false)

let load_interp (panelid : string) (prog: Obc.program) int =
  !stop_fun ();
  let editorid = panelid^"-editor" in
  Page.clear_div editorid;
  match int with
  | Chronogram ->
    let names = List.filter_map (function Pclass cl -> Some cl.cd_name.name | _ -> None) prog.p_desc in
    (try
       Page.create_select editorid names (choose_default names !old_node)
         (fun cname ->
            old_node := Some cname;
            let cls = Obc_interp.find_class prog cname in
            let met = Obc_interp.find_method cls Mstep in
            let mem = ref (Obc_interp.reset prog cname) in

            let reset_fun = fun () ->
              mem := Obc_interp.reset prog cname
            and step_fun = fun inputs ->
              let inputs = List.map2 (fun vd s -> parse_input vd.v_type s) met.m_inputs inputs in
              (* Obc_printer.print_prog Format.std_formatter prog; *)
              let (outputs, new_mem) = Obc_interp.step prog cname inputs !mem in
              mem := new_mem;
              List.map string_of_svalue outputs
            in

            Page.create_hist_table editorid
              (List.map (fun vd -> Idents.source_name vd.v_ident, is_boolean_type vd.v_type) met.m_inputs)
              (List.map (fun vd -> Idents.source_name vd.v_ident, is_boolean_type vd.v_type) met.m_outputs)
              reset_fun step_fun;
            ())
     with _ -> ())
  | Simulator simul ->
    let (module Simul) = simul in
    stop_fun := Simul.stop editorid;
    Simul.start editorid

let interpreter_of_example s p =
  match s with
  (* | "full-adder.lus" -> *)
  (*   Simulator (module Simul.TruthTable) *) (* TODO *)
  | "filters.lus" ->
    Simulator (module Simul.FilterSimul(
                          ObcSimulInterpreter(struct
                              let prog = p
                              let classname = "system"
      end)))
  (* | "stepper.lus" -> Simulator (module Stepper_simul.StepperSimul( *)
  (*       ObcSimulInterpreter(struct *)
  (*         let prog = p *)
  (*         let classname = "motor" *)
  (*       end))) *)
  (* | "porte_telecabine.lus" -> Simulator (module Porte_telecabine_simul.Simul( *)
  (*       ObcSimulInterpreter(struct *)
  (*         let prog = p *)
  (*         let classname = "control_porte" *)
  (*       end) *)
  (*     )) *)
  (* | "chrono.lus" -> Simulator (module Chrono_simul.Simul( *)
  (*       ObcSimulInterpreter(struct *)
  (*         let prog = p *)
  (*         let classname = "main" *)
  (*       end) *)
  (*     )) *)
  | _ -> Chronogram
