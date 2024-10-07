let read_bytes ic =
  let rec aux acc =
    try
      let b = input_byte ic in
      aux (b::acc)
    with _ -> acc
  in List.rev (aux [])



let _ =
  let ifn = Sys.argv.(1) in
  let ic = open_in_bin ifn in
  let bytes = read_bytes ic in
  close_in ic;

  Printf.printf "let %s = [" (Filename.remove_extension ifn);
  List.iter (fun b -> Printf.printf "0x%x;" b) bytes;
  print_string "]\n"
