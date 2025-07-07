let outer x =
  let inner y = x + y in
  inner 5

let () =
  Printf.printf "%d\n" (outer 3)
