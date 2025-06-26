(* Handling of the webpage *)

open Js_of_ocaml
open Js_of_ocaml_tyxml
module T = Tyxml_js.Html5
open Ezjs_ace

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

let remove_first_child divid =
  let div = by_id divid in
  let children = Dom.list_of_nodeList div##.childNodes in
  Dom.removeChild div (List.hd children)

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

(** The next four functions are transferred from tryhept.ml *)

(** Show an error with [text] at [loc] in the console as well as the editor *)
let add_error_marker (editor : unit Ace.editor) (r1, r2) (c1, c2) =
  let open Ace in let open Ace_types in
  (* Printf.fprintf stdout "%d:%d, %d:%d\n" r1 c1 r2 c2; *)
  let range = Ace.range (r1-1) c1 (r2-1) c2 in
  ignore (editor.editor##getSession##addMarker range
            (Js.string "error-marker") (Js.string "text") (Js.bool true))

let parse_loc_message text =
  let reg = Str.regexp "File \"\", line \\([0-9]+\\)-?\\([0-9]+\\)?, characters \\([0-9]+\\)-\\([0-9]+\\):" in
  if not (Str.string_match reg text 0) then invalid_arg "parse_loc_message";
  let r1 = int_of_string (Str.matched_group 1 text) in
  let r2 = try int_of_string (Str.matched_group 2 text) with _ -> r1 in
  let c1 = int_of_string (Str.matched_group 3 text) and c2 = int_of_string (Str.matched_group 4 text) in
  (r1, r2), (c1, c2)

let print_error editor text =
  print_endline text;
  try
    let (row, col) = parse_loc_message text in
    add_error_marker editor row col
  with _ ->
    if text <> "\n" then Console.error text

(** Reset the editor *)
let reset_editor (editor: unit Ace.editor) =
  Ace.clear_marks editor;

  let markers = editor.editor##getSession##getMarkers (Js.bool true) in
  let markers = Js.Unsafe.global##._Object##keys markers in
  let markers = Js.to_array markers in
    Array.iter (fun m -> editor.editor##getSession##removeMarker m) markers;

  Console.clear ()

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

(** The next two functions are transferred from interp.ml *)

let rec string_of_value value =
  match value with
    | Obc_interp.Vbool b -> Bool.to_string b
    | Obc_interp.Vint i -> string_of_int i
    | Obc_interp.Vfloat f -> string_of_float f
    | Obc_interp.Vconstructor c -> c.name
    | Obc_interp.Varray a ->
        let elements = List.map string_of_value (Array.to_list a) in
        "[" ^ (String.concat ", " elements) ^ "]"
    | Obc_interp.Vundef -> "."

let is_boolean_type =
  Types.(function
         | Tid { name = "bool" } -> true
         | _ -> false)

type input_editor_info = {
  reset_fun : unit -> unit;
  step_fun : unit -> Obc_interp.value;
  saved_expression : string;
}

type input_info =
  | Value_from_editor of input_editor_info
  | Manual_value of string list

type inprow_info = {
  var_name : string;
  var_type : Types.ty;
  mutable var_input : input_info;
}

let saved_inprows = ref []

let set_editor_single_line editor =
  ignore (Js.Unsafe.fun_call(Js.Unsafe.js_expr "setEditorSingleLine") [|Js.Unsafe.inject editor|])

let rec create_hist_table divid inps outs reset_fun step_fun =
  print_endline "create_hist_table";

  let headid = "hist-head" in
  let make_first_column =
    List.mapi (fun i (v_name, v_type) ->
      let rowid = "row" ^ string_of_int i in
      rowid, v_name, v_type, T.(tr ~a:[a_id rowid] [th [txt v_name; txt " = "]]))
  in
  let hins = make_first_column inps and houts = make_first_column outs in

  let get_row_input row : Dom_html.inputElement Js.t =
    let opt_get o = Js.Opt.get o (fun _ -> failwith "get_row_input") in
    opt_get (opt_get row##.lastChild)##.firstChild |> Js.Unsafe.coerce in

  (* Get the input values. If they are not all available, raise *)
  let get_latest_inputs () =
    List.map (
      fun (row, info) ->
        let input = get_row_input row in
        if is_boolean_type info.var_type then if Js.to_bool input##.checked then "true" else "false"
        else input##.value |> Js.to_string
    ) !saved_inprows in

  let disable_latest_inputs () =
    List.iter (
      fun (row, _) ->
        let input = get_row_input row in
        input##.disabled := Js.bool true
    ) !saved_inprows in

  let get_row_output row =
    Js.Opt.get row##.lastChild (fun _ -> failwith "get_row_output") in

  let set_latest_outputs houts output =
    List.iter2 (
      fun (rowid, _, v_type, _) s ->
        let cell = get_row_output (by_id rowid) in
        Dom.appendChild cell
          (of_node T.(if is_boolean_type v_type
                      then input ~a:([a_input_type `Checkbox; a_disabled ()]@(if s = "true" then [a_checked ()] else [])) ()
                      else txt s))
    ) houts output in

  let column_number = ref 1 in
  let add_column () =
    Dom.appendChild (by_id headid) (of_node (column_head !column_number));

    List.iter (fun (row, info) ->
      Dom.appendChild row (of_node (input_cell (is_boolean_type info.var_type)));
      let input = get_row_input row in
      match info.var_input with
        | Value_from_editor editor_info ->
          let result = editor_info.step_fun () in
          input##.disabled := Js.bool true;
          if is_boolean_type info.var_type then
            input##.checked := Js.bool (result = Vbool true)
          else
            input##.value := Js.string (string_of_value result)
        | Manual_value l -> ()
          (* Restore previously saved inputs and display them in the table. That ensures that the table is not graphically reset at each new compilation (unless the number of entries or the type of even a single entry changes). *)
(*          (match List.rev l with
            | latest_value :: _ ->
              if is_boolean_type info.var_type then
                input##.checked := Js.bool (latest_value = "true")
              else
                input##.value := Js.string latest_value
            | [] -> ()
          );*)
    ) !saved_inprows;

    (* Restore previously saved inputs and display them in the table. That ensures that the table is not graphically reset at each new compilation (unless the number of entries or the type of even a single entry changes). *)
(*    try
      let inputs = get_latest_inputs () in
      let restored_outputs = step_fun inputs in
      set_latest_outputs restored_outputs;
      disable_latest_inputs ()
    with e -> Console.error (Printexc.to_string e);*)
(*
    List.iter (fun (rowid, _, v_type, _) -> Dom.appendChild (by_id rowid) (of_node (output_cell (is_boolean_type v_type)))) houts;*)

    column_number := !column_number + 1
  in

  let reset_hist_table houts =
    List.iter (fun (_, info) ->
      match info.var_input with
        | Value_from_editor _ -> ()
        | Manual_value _ -> info.var_input <- Manual_value []
    ) !saved_inprows;

    let remove_children parent =
      let children = parent##.childNodes in
      while children##.length > 2 do
        let child_to_remove = Js.Opt.get (children##item 2) (fun _ -> failwith "remove_children") in
        parent##removeChild child_to_remove
      done
    in
    remove_children (Js.Unsafe.coerce (by_id headid));
    List.iter (fun (row, _) -> remove_children (Js.Unsafe.coerce row)) !saved_inprows;
    List.iter (fun (rowid, _, _, _) -> remove_children (by_id rowid)) houts;

    column_number := 1;
    add_column ()
  in

  let step_button =
    T.(button ~a:[
      a_onclick (fun _ ->
        (try
          let inputs = get_latest_inputs () in
          List.iter2 (fun (_, info) input ->
            match info.var_input with
              | Value_from_editor _ -> ()
              | Manual_value l -> info.var_input <- Manual_value (l @ [input])
          ) !saved_inprows inputs;
          let outputs = step_fun inputs in
          set_latest_outputs houts outputs;
          disable_latest_inputs ();
          add_column ()
        with e -> Console.error (Printexc.to_string e));
      true)]
    [txt "step"])
  in

  let reset_button =
    T.(button ~a:[
      a_onclick (fun _ ->
        (try
          reset_hist_table houts
        with e -> Console.error (Printexc.to_string e));
      true)]
    [txt "reset"])
  in

  (* As we added a new cell for each row of inputs, we need to create a initial gap to correctly align the rows of inputs with the head row and the rows of outputs *)
  Dom.appendChild (by_id headid) (of_node T.(th [txt ""]));
  List.iter (fun (rowid, _, v_type, _) -> Dom.appendChild (by_id rowid) (of_node (output_cell (is_boolean_type v_type)))) houts;

  (* Reset the saved inputs list if the saved inputs have changed (ignoring changes in names) *)
  if List.map (fun (_, info) -> info.var_type) !saved_inprows <> List.map (fun (_, v_type) -> v_type) inps
  then (
    let div = by_id divid in
    (try Dom.removeChild div (by_id interp_hist_id) with _ -> ());

    let hhead = T.(tr ~a:[a_id headid] [th [txt ""]]) in
    let tabl = T.(table ~a:[] (hhead :: List.map (fun (_, _, _, x) -> x) (hins @ houts))) in

    (* This is where the DOM elements of the table are created *)
    let interp_div = of_node T.(div ~a:[a_id interp_hist_id] [tabl]) in
    Dom.appendChild div interp_div;
    Dom.appendChild interp_div (of_node step_button);
    Dom.appendChild interp_div (of_node reset_button);

    saved_inprows :=
      List.map (fun (rowid, v_name, v_type, _) ->
        let row = by_id rowid in
        let info =
          { var_name = v_name ;
            var_type = v_type ;
            var_input = Manual_value [] }
        in
        row, info
      ) hins
  )
  (*else saved_inprows :=
    List.map2 (fun (rowid, _, _ ,_) (_, info) ->
      let row = by_id rowid in
      row, info
    ) hins !saved_inprows*);

    print_endline "stop 1";

  (* Add editors in order to put an expression in Heptagon *)
  List.iter (fun (row, info) ->
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
    match info.var_input with
      | Value_from_editor editor_info ->
          editor_info.reset_fun ();
          editor_struct.editor##setValue (Js.string editor_info.saved_expression)
      | Manual_value _ -> ();

    Ace.(editor_struct.editor)##on (Js.string "change") (fun () ->
      Sys_js.set_channel_flusher stderr (fun e -> print_error editor_struct e);
      reset_editor editor_struct;
      let editor_value = Ace.get_contents editor_struct in
      try
        let lexbuf = Lexing.from_string editor_value in
        let program = Compil.build_input_program lexbuf info.var_name (Hept_scoping2.translate_into_hept_parsetree_ty info.var_type) in
        let obc_program = Compil.compile_program "main" program in
        match obc_program.p_desc with
          | [Pclass cls] ->
            let mem = ref (Obc_interp.reset obc_program cls.cd_name.name) in
            info.var_input <- Value_from_editor
              { reset_fun = (fun () ->
                  mem := Obc_interp.reset obc_program cls.cd_name.name);
                step_fun = (fun () ->
                  let inputs = [] in
                  let (outputs, new_mem) = Obc_interp.step obc_program cls.cd_name.name inputs !mem in
                  mem := new_mem;
                  List.hd outputs);
                saved_expression = editor_value };
              reset_hist_table houts
          | _ -> ()
      with Errors.Error -> ()
    )
  ) !saved_inprows;

  print_endline "stop 2";

  (* Add a column to the table *)
  column_number := 1;
  add_column ();

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
  let options = List.map (fun s -> T.(option ~a:[] (txt s))) options in
  let select = of_node T.(select ~a:[] options) in
  let select = Js.Unsafe.coerce select in
  Dom.insertBefore (by_id divid) select (by_id divid)##.firstChild;
  select##.onchange :=
    (fun e -> onselect (Js.to_string select##.value); true);
  select##.value := default;
  onselect default

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
