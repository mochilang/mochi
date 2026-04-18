let is_match s p =
  let i = ref 0 and j = ref 0 and star = ref (-1) and mt = ref 0 in
  while !i < String.length s do
    if !j < String.length p && (p.[!j] = '?' || p.[!j] = s.[!i]) then (incr i; incr j)
    else if !j < String.length p && p.[!j] = '*' then (star := !j; mt := !i; incr j)
    else if !star <> -1 then (j := !star + 1; incr mt; i := !mt)
    else raise Exit
  done;
  while !j < String.length p && p.[!j] = '*' do incr j done;
  !j = String.length p

let () =
  let rec read_all acc = try read_all (read_line () :: acc) with End_of_file -> List.rev acc in
  let lines = read_all [] in
  match lines with
  | [] -> ()
  | x :: _ when String.trim x = "" -> ()
  | t_str :: rest ->
      let t = int_of_string (String.trim t_str) in
      let rec loop tc xs out =
        if tc = t then List.rev out else
          let n = int_of_string (String.trim (List.hd xs)) in
          let xs1 = List.tl xs in
          let s, xs2 = if n > 0 then (List.hd xs1, List.tl xs1) else ("", xs1) in
          let m = int_of_string (String.trim (List.hd xs2)) in
          let xs3 = List.tl xs2 in
          let p, xs4 = if m > 0 then (List.hd xs3, List.tl xs3) else ("", xs3) in
          let ans = try if is_match s p then "true" else "false" with Exit -> "false" in
          loop (tc + 1) xs4 (ans :: out)
      in
      print_string (String.concat "\n" (loop 0 rest []))
