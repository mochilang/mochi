let expand s left right =
  let n = String.length s in
  let rec aux l r =
    if l >= 0 && r < n && s.[l] = s.[r] then aux (l - 1) (r + 1)
    else (l + 1, r - l - 1)
  in
  aux left right

let longest_palindrome s =
  let n = String.length s in
  let best_start = ref 0 in
  let best_len = ref (if n > 0 then 1 else 0) in
  for i = 0 to n - 1 do
    let s1, l1 = expand s i i in
    if l1 > !best_len then (best_start := s1; best_len := l1);
    let s2, l2 = expand s i (i + 1) in
    if l2 > !best_len then (best_start := s2; best_len := l2)
  done;
  String.sub s !best_start !best_len

let () =
  try
    let t = int_of_string (String.trim (read_line ())) in
    let out = ref [] in
    for _ = 1 to t do
      let s = read_line () in
      out := longest_palindrome s :: !out
    done;
    print_string (String.concat "\n" (List.rev !out))
  with End_of_file -> ()
