(* Generated by Mochi transpiler v0.10.33 on 2025-07-21 18:38 +0700 *)

let () =
  let square = (fun x ->
  (x * x)) in
  print_endline (String.concat " " (List.filter (fun s -> s <> "") [string_of_int (square 6)]));
