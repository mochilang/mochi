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

let nations = [[("id",Obj.repr (1));("name",Obj.repr ("A"))];[("id",Obj.repr (2));("name",Obj.repr ("B"))]]
let suppliers = [[("id",Obj.repr (1));("nation",Obj.repr (1))];[("id",Obj.repr (2));("nation",Obj.repr (2))]]
let partsupp = [[("part",Obj.repr (100));("supplier",Obj.repr (1));("cost",Obj.repr (10));("qty",Obj.repr (2))];[("part",Obj.repr (100));("supplier",Obj.repr (2));("cost",Obj.repr (20));("qty",Obj.repr (1))];[("part",Obj.repr (200));("supplier",Obj.repr (1));("cost",Obj.repr (5));("qty",Obj.repr (3))]]
let filtered = (let __res0 = ref [] in
  List.iter (fun ps ->
      List.iter (fun s ->
            List.iter (fun n ->
                        if (Obj.obj (List.assoc "id" s) = Obj.obj (List.assoc "supplier" ps)) && (Obj.obj (List.assoc "id" n) = Obj.obj (List.assoc "nation" s)) && (Obj.obj (List.assoc "name" n) = "A") then
        __res0 := [("part",Obj.repr (Obj.obj (List.assoc "part" ps)));("value",Obj.repr ((Obj.obj (List.assoc "cost" ps) * Obj.obj (List.assoc "qty" ps))))] :: !__res0;
            ) nations;
      ) suppliers;
  ) partsupp;
List.rev !__res0)

let grouped = (let __groups1 = ref [] in
  List.iter (fun x ->
      let key = Obj.obj (List.assoc "part" x) in
      let cur = try List.assoc key !__groups1 with Not_found -> [] in
      __groups1 := (key, x :: cur) :: List.remove_assoc key !__groups1;
  ) filtered;
  let __res1 = ref [] in
  List.iter (fun (gKey,gItems) ->
    let g = { key = gKey; items = List.rev gItems } in
    __res1 := [("part",Obj.repr (g.key));("total",Obj.repr ((sum (let __res2 = ref [] in
  List.iter (fun r ->
      __res2 := r.value :: !__res2;
  ) g;
List.rev !__res2)
)))] :: !__res1
  ) !__groups1;
  List.rev !__res1)


let () =
  print_endline (__show (grouped));
