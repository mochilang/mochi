let union a b = List.sort_uniq compare (a @ b)
let except a b = List.filter (fun x -> not (List.mem x b)) a
let intersect a b = List.filter (fun x -> List.mem x b) a |> List.sort_uniq compare
let union_all a b = a @ b

let pp_list lst =
  Printf.printf "[%s]\n"
    (String.concat "," (List.map string_of_int lst))

let () =
  pp_list (union [1;2] [2;3]);
  pp_list (except [1;2;3] [2]);
  pp_list (intersect [1;2;3] [2;4]);
  Printf.printf "%d\n" (List.length (union_all [1;2] [2;3]))
