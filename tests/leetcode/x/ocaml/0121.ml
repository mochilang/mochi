let max_profit prices =
  match prices with
  | [] -> 0
  | first :: rest ->
      let min_price = ref first and best = ref 0 in
      List.iter (fun p ->
        best := max !best (p - !min_price);
        min_price := min !min_price p
      ) rest;
      !best

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
