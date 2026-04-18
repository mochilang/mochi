let read_all ic = let b = Buffer.create 1024 in (try while true do Buffer.add_channel b ic 1 done with End_of_file -> ()); Buffer.contents b
let fmt xs = "[" ^ String.concat "," (List.map string_of_int xs) ^ "]"
let solve_case s words =
  match words with
  | [] -> []
  | w0::_ ->
      let wlen = String.length w0 in
      let total = wlen * List.length words in
      let target = List.sort compare words in
      let rec loop i acc =
        if i + total > String.length s then List.rev acc else
        let parts = List.init (List.length words) (fun j -> String.sub s (i + j * wlen) wlen) |> List.sort compare in
        loop (i+1) (if parts = target then i::acc else acc)
      in loop 0 []
let () =
  let arr = Array.of_list (String.split_on_char '\n' (read_all stdin)) in
  if Array.length arr = 0 || String.trim arr.(0) = "" then () else
  let idx = ref 0 in let next () = let v = if !idx < Array.length arr then String.trim arr.(!idx) else "" in incr idx; v in
  let t = int_of_string (next ()) in let out = ref [] in
  for _ = 1 to t do let s = next () in let m = int_of_string (next ()) in let words = List.init m (fun _ -> next ()) in out := !out @ [fmt (solve_case s words)] done;
  print_string (String.concat "\n" !out)
