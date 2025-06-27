(** Describe the chronogram *)

type row_info = {
    var_name : string;
    var_type : Types.ty;
    mutable var_values : Obc_interp.value list;
  }

type input_editor_info = {
    reset_fun : unit -> unit;
    step_fun : unit -> Obc_interp.value;
    saved_expression : string;
  }

type input_row_info = {
    row : row_info;
    mutable editor : input_editor_info option;
  }

type t = {
    mutable column_nb : int;
    inputs : input_row_info list;
    outputs : row_info list;
  }

let make inputs outputs =
  let mk_row_info var_name var_type = { var_name; var_type; var_values = [] } in
  {
    column_nb = 1;
    inputs = List.map (fun (name, typ) -> { row = mk_row_info name typ; editor = None }) inputs;
    outputs = List.map (fun (name, typ) -> mk_row_info name typ) outputs;
  }

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
