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


type record1 = { mutable a : int; mutable b : int }

let data : record1 list = [{ a = 1; b = 2 };{ a = 1; b = 1 };{ a = 0; b = 5 }]
let sorted : (string * Obj.t) list list = (let __res0 = ref [] in
  List.iter (fun (x : record1) ->
      __res0 := x :: !__res0;
  ) data;
List.rev !__res0)


let () =
  print_endline (__show (sorted));
