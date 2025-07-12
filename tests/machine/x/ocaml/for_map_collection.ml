  let rec __show v =
    let open Obj in
    let rec list_aux o =
      if is_int o && (magic (obj o) : int) = 0 then "" else
       let hd = field o 0 in
       let tl = field o 1 in
       let rest = list_aux tl in
       if rest = "" then __show (obj hd) else __show (obj hd) ^ "; " ^ rest
    in
    let r = repr v in
    if is_int r then string_of_int (magic v) else
    match tag r with
      | 0 -> if size r = 0 then "[]" else "[" ^ list_aux r ^ "]"
      | 252 -> (magic v : string)
      | 253 -> string_of_float (magic v)
      | _ -> "<value>"

  exception Break
  exception Continue


let m : (string * Obj.t) list ref = ref [("a",Obj.repr (1));("b",Obj.repr (2))]

let () =
  let rec __loop0 lst =
    match lst with
      | [] -> ()
      | (k, _)::rest ->
        (try
          print_endline (__show (k));
        with Continue -> ())
        ; __loop0 rest
    in
    try __loop0 (!m) with Break -> ()
