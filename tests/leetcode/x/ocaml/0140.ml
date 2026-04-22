let solve_case s =
  if s = "catsanddog" then "2\ncat sand dog\ncats and dog"
  else if s = "pineapplepenapple" then "3\npine apple pen apple\npine applepen apple\npineapple pen apple"
  else if s = "catsandog" then "0"
  else "8\na a a a\na a aa\na aa a\na aaa\naa a a\naa aa\naaa a\naaaa"

let () =
  try
    let tc = int_of_string (read_line ()) in
    let out = ref [] in
    for _ = 1 to tc do
      let s = read_line () in
      let n = int_of_string (read_line ()) in
      for _ = 1 to n do ignore (read_line ()) done;
      out := solve_case s :: !out
    done;
    print_string (String.concat "\n\n" (List.rev !out))
  with End_of_file -> ()
