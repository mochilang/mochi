let max_profit prices =
  let rec go xs buy1 sell1 buy2 sell2 =
    match xs with
    | [] -> sell2
    | p :: rest ->
        let buy1 = max buy1 (-p) in
        let sell1 = max sell1 (buy1 + p) in
        let buy2 = max buy2 (sell1 - p) in
        let sell2 = max sell2 (buy2 + p) in
        go rest buy1 sell1 buy2 sell2
  in
  go prices (-1000000000) 0 (-1000000000) 0

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
