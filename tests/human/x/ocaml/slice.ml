let slice lst i j =
  lst |> List.mapi (fun idx x -> idx, x)
      |> List.filter (fun (idx, _) -> idx >= i && idx < j)
      |> List.map snd

let string_slice s i j = String.sub s i (j - i)

let pp_list lst =
  Printf.printf "[%s]\n" (String.concat "," (List.map string_of_int lst))

let () =
  pp_list (slice [1;2;3] 1 3);
  pp_list (slice [1;2;3] 0 2);
  print_endline (string_slice "hello" 1 4)
