(* Handling of the webpage *)

open Js_of_ocaml
open Js_of_ocaml_tyxml
module T = Tyxml_js.Html5

let by_id s = Dom_html.getElementById s
let of_node = Tyxml_js.To_dom.of_node

(** Get a string from local storage *)
let get_string_from_storage loc =
  Js.Optdef.case Dom_html.window##.localStorage
    (fun () -> None)
    (fun stor ->
       Js.Opt.case (stor##getItem (Js.string loc))
         (fun () -> None)
         (fun s -> Some (Js.to_string s)))

(** Save a string in local storage *)
let save_string_in_storage loc s =
  Js.Optdef.case Dom_html.window##.localStorage
    (fun () -> ())
    (fun stor ->
       stor##setItem (Js.string loc) (Js.string s))

(** Remove all children of a div *)
let clear_div divid =
  let div = by_id divid in
  let children = Dom.list_of_nodeList div##.childNodes in
  List.iter (fun n -> Dom.removeChild div n) children

let width divid = (by_id divid)##.offsetWidth

(** Manipulate the console *)
module Console = struct
  (** access to the console *)
  let console = by_id "console"

  (** Scroll the console to the bottom *)
  let scroll child =
    console##.scrollTop := child##.offsetTop

  (** Log a message to the console *)
  let log msg =
    let newLine = of_node (T.(li [txt msg])) in
    ignore (console##appendChild newLine);
    scroll (Js.Unsafe.coerce newLine)

  (** Log an error message to the console *)
  let error msg =
    let newLine = of_node (T.(li ~a:[a_class ["console-error"]] [txt msg])) in
    ignore (console##appendChild newLine);
    scroll (Js.Unsafe.coerce newLine)

  (** Delete all lines on the console *)
  let clear () =
    while Js.to_bool console##hasChildNodes do
      Js.Opt.iter (console##.firstChild) (fun c -> Dom.removeChild console c)
    done
end

(** Generation of fresh variables *)
module Atom = struct
  let counter : int ref = ref 0
  let fresh (s:string) =
    counter := !counter+1; Printf.sprintf "%s%d" s !counter
end

(** Panels *)

type panel_type =
  | Source
  | MiniLS
  | Obc
  | Interpreter

let label_of_panel = function
  | Source -> "Source"
  | MiniLS -> "MiniLS"
  | Obc -> "Obc"
  | Interpreter -> Printf.sprintf "Interpreter"

type control =
  | Button of (unit -> unit)
  | Checkbox of (bool ref)

open Ezjs_ace

type ace_panel = {
  ptype: panel_type;
  controls: (string * control) list;
  editor: unit Ace.editor
}

type panel =
  | AcePanel of string * ace_panel
  | InterpPanel of string

let panel_id = function
  | AcePanel (id, _) -> id
  | InterpPanel id -> id

let output_panels : panel list ref = ref []

let plug_button buttonid f =
  (by_id buttonid)##.onclick := Dom_html.handler (fun _ -> f (); Js._true)

let plug_checkbox id setting =
  (Js.Unsafe.coerce (by_id id))##.checked := Js.bool !setting;
  (by_id id)##.onclick :=
    Dom_html.handler (fun _ ->
        let v = Js.to_bool (Js.Unsafe.coerce (by_id id))##.checked in
        setting := v;
        Js._true)

let plug_control parent (s, c) =
  let id = Atom.fresh "control" in
  match c with
  | Button f ->
    parent##appendChild (of_node T.(input ~a:[a_id id; a_input_type `Button; a_value s] ()));
    (by_id id)##.onclick := Dom_html.handler (fun _ -> f (); Js._true)
  | Checkbox setting ->
    parent##appendChild (of_node T.(input ~a:[a_id id; a_input_type `Checkbox] ()));
    parent##appendChild (of_node T.(label ~a:[a_label_for id] [txt s]));
    plug_checkbox id setting

let rec get_checkbox_value id controls =
  match List.assoc id controls with
  | Checkbox b -> !b
  | _ -> raise Not_found

let remove_panel id =
  Option.iter (function
      | AcePanel (_, p) -> Ace.remove p.editor
      | _ -> ())
    (List.find_opt (fun p -> panel_id p = id) !output_panels);
  output_panels := List.filter (fun p -> panel_id p <> id) !output_panels;
  Dom.removeChild (by_id "body") (by_id id)

let clear_panels () =
  List.iter (function AcePanel (_, p) -> Ace.remove p.editor | _ -> ()) !output_panels;
  List.iter (fun p -> Dom.removeChild (by_id "body") (by_id (panel_id p))) !output_panels;
  output_panels := []

(** Interpreter panel *)

let interp_hist_id = "interpreter-hist"

let column_head n = T.(th [txt (string_of_int n)])
let input_cell isbool =
  T.(td [if isbool then input ~a:[a_input_type `Checkbox] ()
         else input ~a:[a_class ["history"]] ()]
       ~a:[a_class ["history"]])
let output_cell isbool = T.(td [] ~a:[a_class ["history"]])

let saved_inputs = ref []
let saved_inps = ref []

let set_editor_single_line editor =
  ignore (Js.Unsafe.fun_call(Js.Unsafe.js_expr "setEditorSingleLine") [|Js.Unsafe.inject editor|])

let rec create_hist_table divid inps outs reset_fun step_fun =
  let div = by_id divid in
  (try Dom.removeChild div (by_id interp_hist_id) with _ -> ());

  let headid = Atom.fresh "head" in
  let hhead = T.(tr ~a:[a_id headid] [th [txt ""]]) in

  let make_first_column =
    List.map (fun (s, isbool) ->
      let rowid = Atom.fresh "row" in
      rowid, isbool, T.(tr ~a:[a_id rowid] [th [txt s; txt " = "]]))
  in

  let hins = make_first_column inps and houts = make_first_column outs in

  (* Reset the saved inputs list if the saved inputs have changed (ignoring changes in names) *)
  if List.map snd !saved_inps <> List.map snd inps then saved_inputs := [];
  saved_inps := inps;

  let tabl = T.(table ~a:[] (hhead::List.map (fun (_, _, x) -> x) (hins@houts))) in
  let interp_div = of_node T.(div ~a:[a_id interp_hist_id] [tabl]) in
  Dom.appendChild div interp_div;

  (* Functional control of the table *)

  (* Get the parts of the table *)
  let head = by_id headid
  and inprows = List.map (fun (id, isbool, _) -> by_id id, isbool) hins
  and outrows = List.map (fun (id, isbool, _) -> by_id id, isbool) houts in

  let get_row_input row : Dom_html.inputElement Js.t =
    let opt_get o = Js.Opt.get o (fun _ -> failwith "get_row_input") in
    opt_get (opt_get row##.lastChild)##.firstChild |> Js.Unsafe.coerce in

  let editors_structs = ref [] in

  (* Add an editor in order to enter an expression in Heptagon *)
  let add_editor list_of_couples =
    List.iter (fun (row, _) ->
      let input_editor_div_id = Atom.fresh "input-editor" in
      let input_editor_div = T.(div ~a:[a_id input_editor_div_id; a_class ["editor"; "editor-row"]][]) in
      Dom.appendChild row (of_node input_editor_div);
      let editor_struct = Ace.({
        editor_div = by_id input_editor_div_id;
        editor = Ace.edit (by_id input_editor_div_id);
        marks = [];
        keybinding_menu = false
      }) in
      set_editor_single_line editor_struct.editor;
      Ace.set_mode editor_struct "ace/mode/lustre";
      Ace.set_tab_size editor_struct 2;
      editors_structs := !editors_structs @ [editor_struct];
      ()
    ) list_of_couples in

  add_editor inprows;

  (*
  let get_input_editor_struct row =
    let rec find_editor_struct index rows =
      match rows with
        | [] -> failwith "get_input_editor"
        | (r, _) :: _ when r = row -> List.nth !editors_structs index
        | _ :: t -> find_editor_struct (index + 1) t
    in
    find_editor_struct 0 inprows
  in
  *)

  (* As we added a new cell for each row of inputs, we need to create a initial gap to correctly align the rows of inputs with the head row and the rows of outputs *)
  let add_empty_cell list_of_couples =
    List.iter (fun (row, isbool) ->
      Dom.appendChild row (of_node (output_cell isbool))
  ) list_of_couples in

  add_empty_cell [(head, false)];
  add_empty_cell outrows;

  (* Get the input values. If they are not all available, raise *)
  let get_latest_inputs () =
    List.map (
      fun (row, ischeckbox) ->
        let input = get_row_input row in
        if ischeckbox then if Js.to_bool input##.checked then "true" else "false"
        else input##.value |> Js.to_string
    ) inprows in

  let disable_latest_inputs () =
    List.iter (
      fun (row, _) ->
        let input = get_row_input row in
        input##.disabled := Js.bool true
    ) inprows in

  let disable_latest_inputs_editor_not_empty () =
    List.iter2 (
      fun (row, _) editor_struct ->
        let input = get_row_input row in

        let update_state () =
          let editor_value = Js.to_string Ace.(editor_struct.editor##getValue) in
          input##.disabled := Js.bool (editor_value <> "") in

        update_state ();

        Ace.(editor_struct.editor)##on (Js.string "change") (fun () ->
          update_state ()
        )
    ) inprows !editors_structs in

  let get_row_output row =
    Js.Opt.get row##.lastChild (fun _ -> failwith "get_row_output") in

  let set_latest_outputs output =
    List.iter2 (
      fun (row, ischeckbox) s ->
        let cell = get_row_output row in
        Dom.appendChild cell
          (of_node T.(if ischeckbox
                      then input ~a:([a_input_type `Checkbox; a_disabled ()]@(if s = "true" then [a_checked ()] else [])) ()
                      else txt s))
    ) outrows output in

  (* Add a column to the table *)
  let count = ref 1 in
  let add_column () =
    Dom.appendChild head (of_node (column_head !count));
    List.iter (fun (row, isbool) -> Dom.appendChild row (of_node (input_cell isbool))) inprows;
    List.iter (fun (row, isbool) -> Dom.appendChild row (of_node (output_cell isbool))) outrows;
    count := !count + 1;

    (* WARNING: Activate previous cells *)
    disable_latest_inputs_editor_not_empty ()
  in

  (* Restore previously saved inputs and display them in the table. That ensures that the table is not graphically reset at each new compilation (unless the number of entries or the type of even a single entry changes). *)
  let restore_saved_inputs () =
    List.iter (fun inputs ->
      add_column ();
      let rec fill_inputs rows rows_values =
        match rows, rows_values with
          | [], [] -> ()
          | (row, ischeckbox) :: trows, row_value :: trows_values ->
            if ischeckbox then
              (get_row_input row)##.checked := Js.bool (row_value = "true")
            else
              (get_row_input row)##.value := Js.string row_value;
            fill_inputs trows trows_values
          | _ -> ()
      in
      fill_inputs inprows inputs;
      try
        let outputs = step_fun inputs in
        set_latest_outputs outputs;
        disable_latest_inputs ()
      with e -> Console.error (Printexc.to_string e))
    !saved_inputs
  in

  restore_saved_inputs ();
  add_column ();

  let step_button = T.(button ~a:[
      a_onclick (fun _ ->
          (try
             let inputs = get_latest_inputs () in
             saved_inputs := !saved_inputs @ [inputs];
             let outputs = step_fun inputs in
             set_latest_outputs outputs;
             disable_latest_inputs ();
             add_column ()
           with e -> Console.error (Printexc.to_string e));
          true)]
      [txt "step"]) in
  Dom.appendChild interp_div (of_node step_button);

  let reset_button = T.(button ~a:[
      a_onclick (fun _ ->
          (try
             saved_inputs := [];
             create_hist_table divid inps outs reset_fun step_fun
           with e -> Console.error (Printexc.to_string e));
          true)]
      [txt "reset"]) in
  Dom.appendChild interp_div (of_node reset_button);

  reset_fun ()

let create_panel ptype controls =
  let body = by_id "body" in
  let divid = Atom.fresh "panel" in
  let controlsdivid = divid^"-controls" in
  let editordivid = divid^"-editor" in
  ignore
    (body##appendChild (of_node T.(div ~a:[a_id divid; a_class ["panel"]]
                                     [T.(div ~a:[a_id controlsdivid] []);
                                      T.(div ~a:[a_id editordivid; a_class ["editor"]] [])])));
  let readOnly = ptype <> Source in
  (* if readOnly then plug_control (by_id controlsdivid) ("X", Button (fun _ -> remove_panel divid)); *)
  ignore
    ((by_id controlsdivid)##appendChild (of_node T.(span ~a:[a_class ["panel-title"]]
                                                      [txt (label_of_panel ptype)])));
  List.iter (plug_control (by_id controlsdivid)) controls;
  let panel =
    match ptype with
    | Interpreter -> InterpPanel divid
    | _ ->
      let editordiv = by_id editordivid in
      let panel = {
          ptype = ptype;
          controls = controls;
          editor = {
              editor_div = editordiv;
              editor = Ace.edit (by_id editordivid);
              marks = [];
              keybinding_menu = false; }
        } in
      if readOnly then panel.editor.editor##setReadOnly (Js.bool true);
      Ace.set_mode panel.editor "ace/mode/lustre";
      Ace.set_tab_size panel.editor 2;
      AcePanel (divid, panel)
  in
  if readOnly then output_panels := panel::!output_panels;
  panel

let add_panel_control panel control =
  plug_control (by_id ((panel_id panel)^"-controls")) control

let create_select divid (options : string list) default (onselect : string -> unit) =
  let div = by_id divid in
  let options = List.map (fun s -> T.(option ~a:[] (txt s))) options in
  let select = of_node T.(select ~a:[] options) in
  let select = Js.Unsafe.coerce select in
  Dom.appendChild div select;
  select##.onchange :=
    (fun e -> onselect (Js.to_string select##.value); true);
  select##.value := default; onselect default

(** Examples *)

let add_examples load_example =
  let list = by_id "examples" in
  List.iter (fun (name, content) ->
      let id = Atom.fresh "example" in
      Dom.appendChild list (of_node T.(li ~a:[] [button ~a:[a_id id] [txt ("Open "^name)]]));
      plug_button id (fun _ -> load_example name content)
    ) Examples.examples

let create_canvas divid width height : Dom_html.canvasElement Js.t =
  let canvas = of_node T.(canvas ~a:[a_width width; a_height height] []) in
  Dom.appendChild (by_id divid) canvas;
  Js.Unsafe.coerce canvas
