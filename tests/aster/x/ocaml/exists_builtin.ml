(* Generated by Mochi transpiler v0.10.36 on 2025-07-22 16:43 +0700 *)

let () =
  let data = [1; 2] in
  let flag = (List.length (List.filter_map (fun x -> if (x == 1) then Some (x) else None) data) > 0) in
  print_endline (String.concat " " [string_of_bool flag]);
