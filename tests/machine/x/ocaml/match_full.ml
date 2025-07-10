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


let x : int = 2
let label : string = (match x with | 1 -> "one" | 2 -> "two" | 3 -> "three" | _ -> "unknown")
let day : string = "sun"
let mood : string = (match day with | "mon" -> "tired" | "fri" -> "excited" | "sun" -> "relaxed" | _ -> "normal")
let ok : bool = true
let status : string = (match ok with | true -> "confirmed" | false -> "denied")
let rec classify (n : int) : string =
  (match n with | 0 -> "zero" | 1 -> "one" | _ -> "many")


let () =
  print_endline (__show (label));
  print_endline (__show (mood));
  print_endline (__show (status));
  print_endline (__show (classify 0));
  print_endline (__show (classify 5));
