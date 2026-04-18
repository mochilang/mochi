let min_window s t =
  let need = Array.make 128 0 in
  let missing = ref (String.length t) in
  String.iter (fun ch -> need.(Char.code ch) <- need.(Char.code ch) + 1) t;
  let left = ref 0 in
  let best_start = ref 0 in
  let best_len = ref (String.length s + 1) in
  for right = 0 to String.length s - 1 do
    let c = Char.code s.[right] in
    if need.(c) > 0 then decr missing;
    need.(c) <- need.(c) - 1;
    while !missing = 0 do
      if right - !left + 1 < !best_len then begin
        best_start := !left;
        best_len := right - !left + 1
      end;
      let lc = Char.code s.[!left] in
      need.(lc) <- need.(lc) + 1;
      if need.(lc) > 0 then incr missing;
      incr left
    done
  done;
  if !best_len > String.length s then "" else String.sub s !best_start !best_len

let () =
  let rec read_all acc = try read_all (read_line () :: acc) with End_of_file -> List.rev acc in
  match read_all [] with
  | [] -> ()
  | t_str :: rest ->
      let t = int_of_string t_str in
      let rec solve i xs =
        if i = 0 then []
        else
          match xs with
          | s :: p :: tl -> min_window s p :: solve (i - 1) tl
          | _ -> []
      in
      print_string (String.concat "\n" (solve t rest))
