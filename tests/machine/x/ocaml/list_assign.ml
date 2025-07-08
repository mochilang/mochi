let nums = ref [1;2]

let () =
  nums := 3
  print_endline List.nth !nums 1
