let read_all ic = let b = Buffer.create 1024 in (try while true do Buffer.add_channel b ic 1 done with End_of_file -> ()); Buffer.contents b
let fmt xs = "[" ^ String.concat "," (List.map string_of_int xs) ^ "]"
let rec rev_groups xs k = if List.length xs < k then xs else let rec take n acc ys = if n = 0 then (List.rev acc, ys) else match ys with h::t -> take (n-1) (h::acc) t | [] -> (List.rev acc, []) in let a,b = take k [] xs in List.rev a @ rev_groups b k
let () =
  let arr = Array.of_list (String.split_on_char '\n' (read_all stdin)) in
  if Array.length arr = 0 || String.trim arr.(0) = "" then () else
  let idx = ref 0 in
  let next () = let v = if !idx < Array.length arr then String.trim arr.(!idx) else "0" in incr idx; v in
  let t = int_of_string (next ()) in
  let out = ref [] in
  for _ = 1 to t do
    let n = int_of_string (next ()) in
    let vals = ref [] in
    for _ = 1 to n do vals := !vals @ [int_of_string (next ())] done;
    let k = int_of_string (next ()) in
    out := !out @ [fmt (rev_groups !vals k)]
  done;
  print_string (String.concat "\n" !out)
