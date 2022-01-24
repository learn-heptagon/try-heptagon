(* Handling of the webpage *)

open Js_of_ocaml
open Js_of_ocaml_tyxml
module T = Tyxml_js.Html5

let by_id s = Dom_html.getElementById s
let of_node = Tyxml_js.To_dom.of_node

(** Get the program kept in session storage *)
let get_saved_program () =
  Js.Optdef.case Dom_html.window##.localStorage
    (fun () -> "")
    (fun stor ->
       Js.Opt.case (stor##getItem (Js.string "saved_program"))
         (fun () -> "")
         (fun s -> Js.to_string s))

(** Save a program in session storage *)
let save_program prog =
  Js.Optdef.case Dom_html.window##.localStorage
    (fun () -> ())
    (fun stor ->
       stor##setItem (Js.string "saved_program") (Js.string prog))

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

(* Resizable panels *)

type panel_type =
  | Source
  | MiniLS
  | Obc

let label_of_panel = function
  | Source -> "Source"
  | MiniLS -> "MiniLS"
  | Obc -> "Obc"

type control =
  | Button of (unit -> unit)
  | Checkbox of (bool ref)

open Ezjs_ace

type panel = {
  ptype: panel_type;
  id: string;
  controls: (string * control) list;
  editor: unit Ace.editor
}

let output_panels : panel list ref = ref []

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
  Option.iter (fun p -> Ace.remove p.editor) (List.find_opt (fun p -> p.id = id) !output_panels);
  output_panels := List.filter (fun p -> p.id <> id) !output_panels;
  Dom.removeChild (by_id "body") (by_id id)

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
  if readOnly then plug_control (by_id controlsdivid) ("X", Button (fun _ -> remove_panel divid));
  ignore
    ((by_id controlsdivid)##appendChild (of_node T.(span ~a:[a_class ["panel-title"]]
                                                      [txt (label_of_panel ptype)])));
  List.iter (plug_control (by_id controlsdivid)) controls;
  let editordiv = by_id editordivid in
  let panel = {
    ptype = ptype;
    id = divid;
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
  if readOnly then output_panels := panel::!output_panels;
  panel

let add_panel_control panel control =
  plug_control (by_id (panel.id^"-controls")) control
