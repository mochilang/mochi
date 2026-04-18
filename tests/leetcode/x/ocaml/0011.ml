let max_area h =
  let arr = Array.of_list h in
  let left = ref 0 and right = ref (Array.length arr - 1) and best = ref 0 in
  while !left < !right do
    let height = min arr.(!left) arr.(!right) in
    best := max !best ((!right - !left) * height);
    if arr.(!left) < arr.(!right) then incr left else decr right
  done;
  !best

let () =
  try
    let t = int_of_string (read_line ()) in
    let out = ref [] in
    for _ = 1 to t do
      let n = int_of_string (read_line ()) in
      let vals = List.init n (fun _ -> int_of_string (read_line ())) in
      out := string_of_int (max_area vals) :: !out
    done;
    print_string (String.concat "\n" (List.rev !out))
  with End_of_file -> ()
