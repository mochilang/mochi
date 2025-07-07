let x = 3
let y = 4
let m = [ ("a", x); ("b", y) ]

let () =
  Printf.printf "%d %d\n" (List.assoc "a" m) (List.assoc "b" m)
