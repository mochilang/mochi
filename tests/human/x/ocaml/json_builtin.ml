let m = [ ("a", 1); ("b", 2) ]

let json_of_assoc l =
  "{" ^ String.concat "," (List.map (fun (k,v) -> Printf.sprintf "\"%s\":%d" k v) l) ^ "}"

let () =
  print_endline (json_of_assoc m)
