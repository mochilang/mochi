(* Generated by Mochi transpiler v0.10.33 on 2025-07-21 18:38 +0700 *)

let () =
  let nums = ref [1; 2] in
  nums := (List.mapi (fun i x -> if i = 1 then 3 else x) (!nums));
  print_endline (String.concat " " (List.filter (fun s -> s <> "") [string_of_int (List.nth (!nums) 1)]));
