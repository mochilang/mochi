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

let sum lst = List.fold_left (+) 0 lst
type ('k,'v) group = { key : 'k; items : 'v list }

type record1 = { mutable cat : string; mutable val : int }
type record2 = { mutable cat : Obj.t; mutable total : int }

let items : record1 list = [{ cat = "a"; val = 3 };{ cat = "a"; val = 1 };{ cat = "b"; val = 5 };{ cat = "b"; val = 2 }]
let grouped : record2 list = (let __groups0 = ref [] in
  List.iter (fun i ->
      let key = Obj.obj (List.assoc "cat" i) in
      let cur = try List.assoc key !__groups0 with Not_found -> [] in
      __groups0 := (key, i :: cur) :: List.remove_assoc key !__groups0;
  ) items;
  let __res0 = ref [] in
  List.iter (fun (gKey,gItems) ->
    let g = { key = gKey; items = List.rev gItems } in
    __res0 := { cat = g.key; total = (sum (let __res1 = ref [] in
  List.iter (fun x ->
      __res1 := Obj.obj (List.assoc "val" x) :: !__res1;
  ) g.items;
List.rev !__res1)
) } :: !__res0
  ) !__groups0;
  List.rev !__res0)


let () =
  print_endline (__show (grouped));
