let nums = [1; 2; 3]

let () =
  Printf.printf "%b\n" (List.mem 2 nums);
  Printf.printf "%b\n" (List.mem 4 nums)
