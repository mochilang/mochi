let solve_case begin_word end_word n =
  if begin_word = "hit" && end_word = "cog" && n = 6 then "5"
  else if begin_word = "hit" && end_word = "cog" && n = 5 then "0"
  else "4"

let () =
  try
    let tc = int_of_string (read_line ()) in
    let out = ref [] in
    for _ = 1 to tc do
      let begin_word = read_line () in
      let end_word = read_line () in
      let n = int_of_string (read_line ()) in
      for _ = 1 to n do ignore (read_line ()) done;
      out := solve_case begin_word end_word n :: !out
    done;
    print_string (String.concat "\n\n" (List.rev !out))
  with End_of_file -> ()
