let read_all ic = let b = Buffer.create 1024 in (try while true do Buffer.add_channel b ic 1 done with End_of_file -> ()); Buffer.contents b
let valid b r c ch =
  let ok = ref true in
  for i=0 to 8 do if b.(r).(i)=ch || b.(i).(c)=ch then ok:=false done;
  let br=(r/3)*3 and bc=(c/3)*3 in
  for i=br to br+2 do for j=bc to bc+2 do if b.(i).(j)=ch then ok:=false done done; !ok
let rec solve b =
  let pos = ref None in
  for r=0 to 8 do for c=0 to 8 do if b.(r).(c)='.' && !pos=None then pos:=Some(r,c) done done;
  match !pos with None -> true | Some(r,c) -> let rec tryd d = if d > 9 then false else let ch = Char.chr (48+d) in if valid b r c ch then (b.(r).(c)<-ch; if solve b then true else (b.(r).(c)<-'.'; tryd (d+1))) else tryd (d+1) in tryd 1
let () = let arr = Array.of_list (String.split_on_char '\n' (read_all stdin)) in if Array.length arr = 0 || String.trim arr.(0) = "" then () else let idx = ref 0 in let next () = let v = if !idx < Array.length arr then String.trim arr.(!idx) else "" in incr idx; v in let t = int_of_string (next ()) in let out = ref [] in for _ = 1 to t do let b = Array.init 9 (fun _ -> Array.of_seq (String.to_seq (next ()))) in ignore (solve b); for i=0 to 8 do out := !out @ [String.init 9 (fun j -> b.(i).(j))] done done; print_string (String.concat "\n" !out)
