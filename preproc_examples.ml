(* Preprocess the examples files to add them in the examples.ml file *)

let example_dir = "examples"

let escape s =
  String.map
    (function '-' -> '_' | c -> c)
    (String.escaped s)

let read_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

let open_and_read () =
  let files = Array.to_list (Sys.readdir example_dir) in
  List.map (fun f -> (f, read_file (example_dir^"/"^f))) files

let print_content pf name content =
  Format.fprintf pf "let %s = \"\\\n%s\"\n\n"
    name content

let print_table_entry pf name =
  Format.fprintf pf "(\"%s\", %s)" name (escape (Filename.remove_extension name))

let print_semicol_list p =
  Format.pp_print_list ~pp_sep:(fun p () -> Format.fprintf p ";") p

let print_table pf files =
  Format.fprintf pf "let examples = [%a]\n"
    (print_semicol_list (fun pf (name, _) -> print_table_entry pf name)) files

let _ =
  let files = open_and_read () in
  let f = open_out "examples.ml" in
  let pf = Format.formatter_of_out_channel f in
  let files = List.filter (fun (name, _) -> Filename.extension name = ".lus") files in
  List.iter (fun (name, content) ->
      print_content pf (escape (Filename.remove_extension name)) content
    ) files;
  print_table pf files;
  close_out f
