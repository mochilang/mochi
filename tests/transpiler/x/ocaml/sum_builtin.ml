(* Generated by Mochi transpiler v0.10.33 on 2025-07-21 18:38 +0700 *)

let () =
  print_endline (String.concat " " (List.filter (fun s -> s <> "") [string_of_int (List.fold_left (fun acc x -> acc + x) 0 [1; 2; 3])]));
