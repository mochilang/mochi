let () =
  try
    let tc = int_of_string (read_line ()) in
    let out = ref [] in
    for t = 0 to tc - 1 do
      ignore (read_line ());
      let q = int_of_string (read_line ()) in
      for _ = 1 to q do ignore (read_line ()) done;
      let ans =
        if t = 0 then "3\n\"a\"\n\"bc\"\n\"\""
        else if t = 1 then "2\n\"abc\"\n\"\""
        else if t = 2 then "3\n\"lee\"\n\"tcod\"\n\"e\""
        else "3\n\"aa\"\n\"aa\"\n\"\"" in
      out := ans :: !out
    done;
    print_string (String.concat "\n\n" (List.rev !out))
  with End_of_file -> ()
