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

let items = [[("cat",Obj.repr ("a"));("val",Obj.repr (10));("flag",Obj.repr (true))];[("cat",Obj.repr ("a"));("val",Obj.repr (5));("flag",Obj.repr (false))];[("cat",Obj.repr ("b"));("val",Obj.repr (20));("flag",Obj.repr (true))]]
let result = (let __groups0 = ref [] in
  List.iter (fun i ->
      let key = Obj.obj (List.assoc "cat" i) in
      let cur = try List.assoc key !__groups0 with Not_found -> [] in
      __groups0 := (key, i :: cur) :: List.remove_assoc key !__groups0;
  ) items;
  let __res0 = ref [] in
  List.iter (fun (gKey,gItems) ->
    let g = { key = gKey; items = List.rev gItems } in
    __res0 := [("cat",Obj.repr (g.key));("share",Obj.repr (((sum (let __res1 = ref [] in
  List.iter (fun x ->
      __res1 := (if x.flag then x.val else 0) :: !__res1;
  ) g;
List.rev !__res1)
) / (sum (let __res2 = ref [] in
  List.iter (fun x ->
      __res2 := x.val :: !__res2;
  ) g;
List.rev !__res2)
))))] :: !__res0
  ) !__groups0;
  List.rev !__res0)


let () =
  print_endline (__show (result));
