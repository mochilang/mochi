let xs = [1; 2; 3]
let ys = List.filter (fun x -> x mod 2 = 1) xs

let m = [ ("a", 1) ]

let substring s sub =
  let len_s = String.length s and len_sub = String.length sub in
  let rec aux i =
    if i + len_sub > len_s then false
    else if String.sub s i len_sub = sub then true
    else aux (i + 1)
  in
  aux 0

let s = "hello"

let () =
  Printf.printf "%b\n" (List.mem 1 ys);
  Printf.printf "%b\n" (List.mem 2 ys);
  Printf.printf "%b\n" (List.assoc_opt "a" m <> None);
  Printf.printf "%b\n" (List.assoc_opt "b" m <> None);
  Printf.printf "%b\n" (substring s "ell");
  Printf.printf "%b\n" (substring s "foo")
