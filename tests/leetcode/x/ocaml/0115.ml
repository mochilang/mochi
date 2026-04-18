let solve s t =
  let n = String.length t in
  let dp = Array.make (n + 1) 0 in
  dp.(0) <- 1;
  String.iter (fun ch ->
    for j = n downto 1 do
      if ch = t.[j - 1] then dp.(j) <- dp.(j) + dp.(j - 1)
    done
  ) s;
  dp.(n)

let () =
  try
    let tc = int_of_string (read_line ()) in
    let out = ref [] in
    for _ = 1 to tc do
      let s = read_line () in
      let t = read_line () in
      out := string_of_int (solve s t) :: !out
    done;
    print_string (String.concat "\n" (List.rev !out))
  with End_of_file -> ()
