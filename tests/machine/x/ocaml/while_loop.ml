let i = ref 0

let () =
  let rec loop () =
    if (!i < 3) then (
      print_endline !i
      i := (!i + 1)
      loop ()
    ) else ()
  in loop ()
