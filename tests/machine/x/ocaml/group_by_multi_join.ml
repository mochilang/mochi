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

type record1 = { mutable id : int; mutable name : string }
type record2 = { mutable id : int; mutable nation : int }
type record3 = { mutable part : int; mutable supplier : int; mutable cost : float; mutable qty : int }
type record4 = { mutable part : Obj.t; mutable value : Obj.t }
type record5 = { mutable part : Obj.t; mutable total : int }

let nations : record1 list = [{ id = 1; name = "A" };{ id = 2; name = "B" }]
let suppliers : record2 list = [{ id = 1; nation = 1 };{ id = 2; nation = 2 }]
let partsupp : record3 list = [{ part = 100; supplier = 1; cost = 10; qty = 2 };{ part = 100; supplier = 2; cost = 20; qty = 1 };{ part = 200; supplier = 1; cost = 5; qty = 3 }]
let filtered : record4 list = (let __res0 = ref [] in
  List.iter (fun ps ->
      List.iter (fun s ->
            List.iter (fun n ->
                        if (Obj.obj (List.assoc "id" s) = Obj.obj (List.assoc "supplier" ps)) && (Obj.obj (List.assoc "id" n) = Obj.obj (List.assoc "nation" s)) && (Obj.obj (List.assoc "name" n) = "A") then
        __res0 := { part = Obj.obj (List.assoc "part" ps); value = (Obj.obj (List.assoc "cost" ps) * Obj.obj (List.assoc "qty" ps)) } :: !__res0;
            ) nations;
      ) suppliers;
  ) partsupp;
List.rev !__res0)

let grouped : record5 list = (let __groups1 = ref [] in
  List.iter (fun x ->
      let key = Obj.obj (List.assoc "part" x) in
      let cur = try List.assoc key !__groups1 with Not_found -> [] in
      __groups1 := (key, x :: cur) :: List.remove_assoc key !__groups1;
  ) filtered;
  let __res1 = ref [] in
  List.iter (fun (gKey,gItems) ->
    let g = { key = gKey; items = List.rev gItems } in
    __res1 := { part = g.key; total = (sum (let __res2 = ref [] in
  List.iter (fun r ->
      __res2 := Obj.obj (List.assoc "value" r) :: !__res2;
  ) g.items;
List.rev !__res2)
) } :: !__res1
  ) !__groups1;
  List.rev !__res1)


let () =
  print_endline (__show (grouped));
