let nums = [1; 2]
let letters = ["A"; "B"]
let bools = [true; false]

let combos =
  List.concat (List.map (fun n ->
    List.concat (List.map (fun l ->
      List.map (fun b -> (n,l,b)) bools
    ) letters)
  ) nums)

let () =
  print_endline "--- Cross Join of three lists ---";
  List.iter (fun (n,l,b) -> Printf.printf "%d %s %b\n" n l b) combos
