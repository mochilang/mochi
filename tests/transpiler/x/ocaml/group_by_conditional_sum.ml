(* Generated by Mochi transpiler v0.10.33 on 2025-07-21 18:38 +0700 *)

let () =
  let items = [[("cat", "a"); ("val", 10); ("flag", true)]; [("cat", "a"); ("val", 5); ("flag", false)]; [("cat", "b"); ("val", 20); ("flag", true)]] in
  let result = (let __groups0 = ref [] in
  List.iter (fun i ->
    let key = (List.assoc "cat" i) in
    let cur = try List.assoc key !__groups0 with Not_found -> [] in
    __groups0 := (key, i :: cur) :: List.remove_assoc key !__groups0;
  ) items;
  let __res0 = ref [] in
  List.iter (fun (g_key, g_items) ->
    let g = List.rev g_items in
    __res0 := [("cat", g_key); ("share", ((List.fold_left (fun acc x -> acc + x) 0 (List.map (fun x -> if (List.assoc "flag" x) then (List.assoc "val" x) else 0) g)) / (List.fold_left (fun acc x -> acc + x) 0 (List.map (fun x -> (List.assoc "val" x)) g))))] :: !__res0
  ) !__groups0;
  List.rev !__res0) in
  print_endline (String.concat " " (List.filter (fun s -> s <> "") [result]));
