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

exception Break
exception Continue

let string_contains s sub =
  let len_s = String.length s and len_sub = String.length sub in
  let rec aux i =
    if i + len_sub > len_s then false
    else if String.sub s i len_sub = sub then true
    else aux (i + 1)
  in aux 0

let slice lst i j =
  lst |> List.mapi (fun idx x -> idx, x)
      |> List.filter (fun (idx, _) -> idx >= i && idx < j)
      |> List.map snd

let string_slice s i j = String.sub s i (j - i)

let list_set lst idx value =
  List.mapi (fun i v -> if i = idx then value else v) lst

let rec map_set m k v =
  match m with
    | [] -> [(k,Obj.repr v)]
    | (k2,v2)::tl -> if k2 = k then (k,Obj.repr v)::tl else (k2,v2)::map_set tl k v

let map_get m k = Obj.obj (List.assoc k m)

let list_union a b = List.sort_uniq compare (a @ b)
let list_except a b = List.filter (fun x -> not (List.mem x b)) a
let list_intersect a b = List.filter (fun x -> List.mem x b) a |> List.sort_uniq compare
let list_union_all a b = a @ b
let sum lst = List.fold_left (+) 0 lst

let nations = [[("id",Obj.repr 1);("name",Obj.repr "A")];[("id",Obj.repr 2);("name",Obj.repr "B")]]
let suppliers = [[("id",Obj.repr 1);("nation",Obj.repr 1)];[("id",Obj.repr 2);("nation",Obj.repr 2)]]
let partsupp = [[("part",Obj.repr 100);("supplier",Obj.repr 1);("cost",Obj.repr 10);("qty",Obj.repr 2)];[("part",Obj.repr 100);("supplier",Obj.repr 2);("cost",Obj.repr 20);("qty",Obj.repr 1)];[("part",Obj.repr 200);("supplier",Obj.repr 1);("cost",Obj.repr 5);("qty",Obj.repr 3)]]
let filtered = (let __res0 = ref [] in
  List.iter (fun ps ->
      List.iter (fun s ->
            List.iter (fun n ->
                        if (s.id = ps.supplier) && (n.id = s.nation) && (n.name = "A") then
        __res0 := [("part",Obj.repr ps.part);("value",Obj.repr (ps.cost * ps.qty))] :: !__res0;
            ) nations;
      ) suppliers;
  ) partsupp;
List.rev !__res0)

let grouped = (let __res1 = ref [] in
  List.iter (fun x ->
      __res1 := [("part",Obj.repr g.key);("total",Obj.repr (sum (let __res2 = ref [] in
  List.iter (fun r ->
      __res2 := r.value :: !__res2;
  ) g;
List.rev !__res2)
))] :: !__res1;
  ) filtered;
List.rev !__res1)


let () =
  print_endline (__show (grouped));
