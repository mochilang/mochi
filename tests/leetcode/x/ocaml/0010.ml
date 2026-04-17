let rec match_at s p i j =
  if j >= String.length p then i >= String.length s
  else
    let first = i < String.length s && (p.[j] = '.' || s.[i] = p.[j]) in
    if j + 1 < String.length p && p.[j + 1] = '*' then
      match_at s p i (j + 2) || (first && match_at s p (i + 1) j)
    else
      first && match_at s p (i + 1) (j + 1)

let () =
  try
    let t = int_of_string (input_line stdin) in
    let out = ref [] in
    for _ = 1 to t do
      let s = input_line stdin in
      let p = input_line stdin in
      out := !out @ [if match_at s p 0 0 then "true" else "false"]
    done;
    print_string (String.concat "\n" !out)
  with End_of_file -> ()
