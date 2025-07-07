type counter = { mutable n : int }

let inc c = c.n <- c.n + 1

let () =
  let c = { n = 0 } in
  inc c;
  Printf.printf "%d\n" c.n
