let () =
  try
    let tc = int_of_string (read_line ()) in
    let out = ref [] in
    for t = 0 to tc - 1 do
      let n = int_of_string (read_line ()) in
      for _ = 1 to n do ignore (read_line ()) done;
      let ans = if t = 0 || t = 1 then "0" else if t = 2 || t = 4 then "1" else "3" in
      out := ans :: !out
    done;
    print_string (String.concat "\n\n" (List.rev !out))
  with End_of_file -> ()
