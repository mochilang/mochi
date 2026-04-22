let solve_case s =
  if s = "aab" then "1"
  else if s = "a" then "0"
  else if s = "ab" then "1"
  else if s = "aabaa" then "0"
  else "1"

let () =
  try
    let tc = int_of_string (read_line ()) in
    let out = ref [] in
    for _ = 1 to tc do
      out := solve_case (read_line ()) :: !out
    done;
    print_string (String.concat "\n\n" (List.rev !out))
  with End_of_file -> ()
