let solve s1 s2 s3 =
  let m = String.length s1 and n = String.length s2 in
  if m + n <> String.length s3 then false
  else
    let dp = Array.make_matrix (m + 1) (n + 1) false in
    dp.(0).(0) <- true;
    for i = 0 to m do
      for j = 0 to n do
        if i > 0 && dp.(i-1).(j) && s1.[i-1] = s3.[i+j-1] then dp.(i).(j) <- true;
        if j > 0 && dp.(i).(j-1) && s2.[j-1] = s3.[i+j-1] then dp.(i).(j) <- true
      done
    done;
    dp.(m).(n)

let () =
  let rec read_all acc = try read_all (read_line () :: acc) with End_of_file -> List.rev acc in
  match read_all [] with
  | [] -> ()
  | t_str :: rest ->
      let t = int_of_string t_str in
      let rec run i xs acc =
        if i = t then List.rev acc
        else match xs with s1 :: s2 :: s3 :: tl -> run (i+1) tl ((if solve s1 s2 s3 then "true" else "false") :: acc) | _ -> List.rev acc
      in print_string (String.concat "\n" (run 0 rest []))
