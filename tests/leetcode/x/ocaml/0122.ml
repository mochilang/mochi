let max_profit prices =
  let rec go xs best =
    match xs with
    | a :: ((b :: _) as rest) -> go rest (if b > a then best + (b - a) else best)
    | _ -> best
  in
  go prices 0

let () =
  try
    let t = int_of_string (read_line ()) in
    let out = ref [] in
    for _ = 1 to t do
      let n = int_of_string (read_line ()) in
      let prices = List.init n (fun _ -> int_of_string (read_line ())) in
      out := string_of_int (max_profit prices) :: !out
    done;
    print_string (String.concat "\n" (List.rev !out))
  with End_of_file -> ()
