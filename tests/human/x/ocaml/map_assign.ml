let scores : (string, int) Hashtbl.t = Hashtbl.create 10
let () =
  Hashtbl.add scores "alice" 1;
  Hashtbl.replace scores "bob" 2;
  print_endline (string_of_int (Hashtbl.find scores "bob"))
