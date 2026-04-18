let median a b =
  let rec merge a b = match a, b with
    | [], x | x, [] -> x
    | ah::at, bh::bt -> if ah <= bh then ah :: merge at b else bh :: merge a bt
  in
  let m = Array.of_list (merge a b) in
  if Array.length m mod 2 = 1 then float_of_int m.(Array.length m / 2)
  else float_of_int (m.(Array.length m / 2 - 1) + m.(Array.length m / 2)) /. 2.0

let () =
  try
    let t = int_of_string (read_line ()) in
    let out = ref [] in
    for _ = 1 to t do
      let n = int_of_string (read_line ()) in
      let a = List.init n (fun _ -> int_of_string (read_line ())) in
      let m = int_of_string (read_line ()) in
      let b = List.init m (fun _ -> int_of_string (read_line ())) in
      out := Printf.sprintf "%.1f" (median a b) :: !out
    done;
    print_string (String.concat "\n" (List.rev !out))
  with End_of_file -> ()
