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

let rec map_set m k v =
  match m with
    | [] -> [(k,Obj.repr v)]
    | (k2,v2)::tl -> if k2 = k then (k,Obj.repr v)::tl else (k2,v2)::map_set tl k v

let map_get m k = Obj.obj (List.assoc k m)


type record1 = { mutable outer : (string * Obj.t) list }
type record2 = { mutable inner : int }

let data : (string * Obj.t) list ref = ref { outer = { inner = 1 } }

let () =
  data := map_set !data "outer" (map_set (map_get !data "outer") "inner" 2);
  print_endline (__show (Obj.obj (List.assoc "inner" Obj.obj (List.assoc "outer" (!data)))));
