let int_min = -2147483648
let int_max = 2147483647

let reverse_int x =
  let rec go x ans =
    if x = 0 then ans
    else
      let digit = x mod 10 in
      let x = x / 10 in
      if ans > int_max / 10 || (ans = int_max / 10 && digit > 7) then 0
      else if ans < int_min / 10 || (ans = int_min / 10 && digit < -8) then 0
      else go x (ans * 10 + digit)
  in
  go x 0

let () =
  try
    let t = int_of_string (String.trim (read_line ())) in
    let out = ref [] in
    for _ = 1 to t do
      let x = int_of_string (String.trim (read_line ())) in
      out := string_of_int (reverse_int x) :: !out
    done;
    print_string (String.concat "\n" (List.rev !out))
  with End_of_file -> ()
