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

let string_slice s i j = String.sub s i (j - i)


let prefix = "fore"
let s1 = "forest"
let s2 = "desert"

let () =
  print_endline (__show ((string_slice s1 0 (String.length prefix) = prefix)));
  print_endline (__show ((string_slice s2 0 (String.length prefix) = prefix)));
