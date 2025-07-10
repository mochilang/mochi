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


let numbers : int list = [1;2;3;4;5;6;7;8;9]

let () =
  let rec __loop0 lst =
    match lst with
      | [] -> ()
      | n::rest ->
        try
          if ((n mod 2) = 0) then (
            raise Continue
          ) ;
          if (n > 7) then (
            raise Break
          ) ;
          print_endline (__show ("odd number:") ^ " " ^ __show (n));
        with Continue -> ()
        ; __loop0 rest
    in
    try __loop0 numbers with Break -> ()
