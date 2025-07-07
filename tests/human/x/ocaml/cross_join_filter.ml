let nums = [1; 2; 3]
let letters = ["A"; "B"]

let pairs =
  List.filter (fun (n, _) -> n mod 2 = 0)
    (List.concat (List.map (fun n -> List.map (fun l -> (n, l)) letters) nums))

let () =
  print_endline "--- Even pairs ---";
  List.iter (fun (n,l) -> Printf.printf "%d %s\n" n l) pairs
