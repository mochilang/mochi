let substring s sub =
  let len_s = String.length s and len_sub = String.length sub in
  let rec aux i =
    if i + len_sub > len_s then false
    else if String.sub s i len_sub = sub then true
    else aux (i + 1)
  in
  aux 0

let s = "catch"

let () =
  Printf.printf "%b\n" (substring s "cat");
  Printf.printf "%b\n" (substring s "dog")
