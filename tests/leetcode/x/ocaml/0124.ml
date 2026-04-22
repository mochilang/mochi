let solve vals ok =
  let n = Array.length vals in
  let best = ref (-1000000000) in
  let rec dfs i =
    if i >= n || not ok.(i) then 0
    else
      let left = max 0 (dfs (2 * i + 1)) in
      let right = max 0 (dfs (2 * i + 2)) in
      best := max !best (vals.(i) + left + right);
      vals.(i) + max left right
  in
  ignore (dfs 0);
  !best

let () =
  try
    let tc = int_of_string (read_line ()) in
    let out = ref [] in
    for _ = 1 to tc do
      let n = int_of_string (read_line ()) in
      let vals = Array.make n 0 in
      let ok = Array.make n false in
      for i = 0 to n - 1 do
        let tok = read_line () in
        if tok <> "null" then (ok.(i) <- true; vals.(i) <- int_of_string tok)
      done;
      out := string_of_int (solve vals ok) :: !out
    done;
    print_string (String.concat "\n" (List.rev !out))
  with End_of_file -> ()
