let prefix = "fore"
let len_prefix = String.length prefix

let s1 = "forest"
let s2 = "desert"

let () =
  Printf.printf "%b\n" (String.sub s1 0 len_prefix = prefix);
  Printf.printf "%b\n" (String.sub s2 0 len_prefix = prefix)
