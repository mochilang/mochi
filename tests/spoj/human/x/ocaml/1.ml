(* [Life, the Universe, and Everything](https://www.spoj.com/problems/TEST) *)

let rec loop () =
  try
    let n = read_int () in
    if n = 42 then ()
    else (
      print_int n;
      print_newline ();
      loop ())
  with End_of_file -> ()

let () = loop ()
