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


module Math = struct
  let pi = Float.pi
  let e = exp 1.0
  let sqrt x = sqrt x
  let pow x y = x ** y
  let sin x = sin x
  let log x = log x
end
let r : float = 3.
let area : float = (Math.pi *. Math.pow r 2.)
let root : float = Math.sqrt 49.
let sin45 : float = Math.sin (Math.pi /. 4.)
let log_e : float = Math.log Math.e

let () =
  print_endline (__show ("Circle area with r =") ^ " " ^ __show (r) ^ " " ^ __show ("=>") ^ " " ^ __show (area));
  print_endline (__show ("Square root of 49:") ^ " " ^ __show (root));
  print_endline (__show ("sin(π/4):") ^ " " ^ __show (sin45));
  print_endline (__show ("log(e):") ^ " " ^ __show (log_e));
