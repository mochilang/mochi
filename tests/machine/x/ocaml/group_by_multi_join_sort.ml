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
let join_strings parts sep = String.concat sep parts

let nation = [[("n_nationkey",Obj.repr 1);("n_name",Obj.repr "BRAZIL")]]
let customer = [[("c_custkey",Obj.repr 1);("c_name",Obj.repr "Alice");("c_acctbal",Obj.repr 100);("c_nationkey",Obj.repr 1);("c_address",Obj.repr "123 St");("c_phone",Obj.repr "123-456");("c_comment",Obj.repr "Loyal")]]
let orders = [[("o_orderkey",Obj.repr 1000);("o_custkey",Obj.repr 1);("o_orderdate",Obj.repr "1993-10-15")];[("o_orderkey",Obj.repr 2000);("o_custkey",Obj.repr 1);("o_orderdate",Obj.repr "1994-01-02")]]
let lineitem = [[("l_orderkey",Obj.repr 1000);("l_returnflag",Obj.repr "R");("l_extendedprice",Obj.repr 1000);("l_discount",Obj.repr 0.1)];[("l_orderkey",Obj.repr 2000);("l_returnflag",Obj.repr "N");("l_extendedprice",Obj.repr 500);("l_discount",Obj.repr 0)]]
let start_date = "1993-10-01"
let end_date = "1994-01-01"
let result = (let __res0 = ref [] in
  List.iter (fun c ->
      List.iter (fun o ->
            List.iter (fun l ->
                    List.iter (fun n ->
                                    if (o.o_custkey = c.c_custkey) && (l.l_orderkey = o.o_orderkey) && (n.n_nationkey = c.c_nationkey) && (((((o.o_orderdate >= start_date) && o.o_orderdate) < end_date) && l.l_returnflag) = "R") then
          __res0 := [("c_custkey",Obj.repr g.key.c_custkey);("c_name",Obj.repr g.key.c_name);("revenue",Obj.repr (sum (let __res1 = ref [] in
  List.iter (fun x ->
      __res1 := (x.l.l_extendedprice * ((1 - x.l.l_discount))) :: !__res1;
  ) g;
List.rev !__res1)
));("c_acctbal",Obj.repr g.key.c_acctbal);("n_name",Obj.repr g.key.n_name);("c_address",Obj.repr g.key.c_address);("c_phone",Obj.repr g.key.c_phone);("c_comment",Obj.repr g.key.c_comment)] :: !__res0;
                    ) nation;
            ) lineitem;
      ) orders;
  ) customer;
List.rev !__res0)


let () =
  print_endline (__show (result));
