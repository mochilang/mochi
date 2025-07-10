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


type record1 = { mutable n : int; mutable v : string }

let items : record1 list = [{ n = 1; v = "a" };{ n = 1; v = "b" };{ n = 2; v = "c" }]
let result : Obj.t list = (let __res0 = ref [] in
  List.iter (fun i ->
      __res0 := Obj.obj (List.assoc "v" i) :: !__res0;
  ) items;
List.rev !__res0)


let () =
  print_endline (__show (result));
