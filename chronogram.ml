(** Describe the chronogram *)

type row_info = {
    var_name : string;
    var_type : Types.ty;
    mutable var_values : Obc_interp.value list;
  }

type input_editor_info = {
    reset_fun : unit -> unit;
    step_fun : int -> Obc_interp.value;
    saved_expression : string;
  }

type input_row_info = {
    row : row_info;
    mutable editor : input_editor_info option;
  }

type t = {
    inputs : input_row_info list;
    outputs : row_info list;
  }

let make inputs outputs =
  let mk_row_info var_name var_type = { var_name; var_type; var_values = [] } in
  {
    inputs = List.map (fun (name, typ) -> { row = mk_row_info name typ; editor = None }) inputs;
    outputs = List.map (fun (name, typ) -> mk_row_info name typ) outputs;
  }

let rec recompute_outputs step_fun st =
  List.iter (fun info -> info.var_values <- []) st.outputs;
  let input_values_lists = List.map (fun info -> info.row.var_values) st.inputs in
  let rec browse_columns list_of_lists =
    match list_of_lists with
      | [] -> ()
      | l :: _ ->
        (* If the first list is empty, then all lists are empty. *)
        if l = [] then ()
        else
          (* Analogically corresponds to the "current column" of inputs (respectively, outputs), i.e., the column of inputs (respectively, outputs) for the current index. *)
          let current_inputs = List.map List.hd list_of_lists in
          let current_outputs = step_fun current_inputs in

          List.iter2 (fun info v ->
            info.var_values <- info.var_values @ [v]
          ) st.outputs current_outputs;

          let remaining_columns = List.map List.tl list_of_lists in
          browse_columns remaining_columns
  in
  browse_columns input_values_lists

open Obc_interp

let rec string_of_value value =
  match value with
    | Vbool b -> Bool.to_string b
    | Vint i -> string_of_int i
    | Vfloat f -> string_of_float f
    | Vconstructor c -> c.name
    | Varray a ->
        let elements = List.map string_of_value (Array.to_list a) in
        "[" ^ (String.concat ", " elements) ^ "]"
    | Vundef -> "."

exception ParseInputError of string
let () =
  Printexc.register_printer
    (function
      | ParseInputError v -> Some (Printf.sprintf "Couldn't parse input %s" v)
      | _ -> None)

let parse_input typ s =
  try
    if s = "." then Vundef
    else
      let open Types in
      match typ with
      | Tid { name = "bool" } -> Vbool (s = "true")
      | Tid { name = "int" } -> Vint (int_of_string s)
      | Tid { name = "real" } | Tid { name = "float" } -> Vfloat (float_of_string s)
      | _ -> failwith "TODO"
  with _ -> raise (ParseInputError s)
