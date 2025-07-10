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

let items = [[("cat",Obj.repr ("a"));("val",Obj.repr (3))];[("cat",Obj.repr ("a"));("val",Obj.repr (1))];[("cat",Obj.repr ("b"));("val",Obj.repr (5))];[("cat",Obj.repr ("b"));("val",Obj.repr (2))]]
let grouped = (let __groups0 = ref [] in
  List.iter (fun i ->
      let key = Obj.obj (List.assoc "cat" i) in
      let cur = try List.assoc key !__groups0 with Not_found -> [] in
      __groups0 := (key, i :: cur) :: List.remove_assoc key !__groups0;
  ) items;
  let __res0 = ref [] in
  List.iter (fun (gKey,gItems) ->
    let g = { key = gKey; items = List.rev gItems } in
    __res0 := [("cat",Obj.repr (g.key));("total",Obj.repr ((sum (let __res1 = ref [] in
  List.iter (fun x ->
      __res1 := x.val :: !__res1;
  ) g;
List.rev !__res1)
)))] :: !__res0
  ) !__groups0;
  List.rev !__res0)


let () =
  print_endline (__show (grouped));
