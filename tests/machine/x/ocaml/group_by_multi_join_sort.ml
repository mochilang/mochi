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

let nation = [[("n_nationkey",Obj.repr (1));("n_name",Obj.repr ("BRAZIL"))]]
let customer = [[("c_custkey",Obj.repr (1));("c_name",Obj.repr ("Alice"));("c_acctbal",Obj.repr (100));("c_nationkey",Obj.repr (1));("c_address",Obj.repr ("123 St"));("c_phone",Obj.repr ("123-456"));("c_comment",Obj.repr ("Loyal"))]]
let orders = [[("o_orderkey",Obj.repr (1000));("o_custkey",Obj.repr (1));("o_orderdate",Obj.repr ("1993-10-15"))];[("o_orderkey",Obj.repr (2000));("o_custkey",Obj.repr (1));("o_orderdate",Obj.repr ("1994-01-02"))]]
let lineitem = [[("l_orderkey",Obj.repr (1000));("l_returnflag",Obj.repr ("R"));("l_extendedprice",Obj.repr (1000));("l_discount",Obj.repr (0.1))];[("l_orderkey",Obj.repr (2000));("l_returnflag",Obj.repr ("N"));("l_extendedprice",Obj.repr (500));("l_discount",Obj.repr (0))]]
let start_date = "1993-10-01"
let end_date = "1994-01-01"
let result = (let __groups0 = ref [] in
  List.iter (fun c ->
      List.iter (fun o ->
            List.iter (fun l ->
                    List.iter (fun n ->
                                    if (Obj.obj (List.assoc "o_custkey" o) = Obj.obj (List.assoc "c_custkey" c)) && (Obj.obj (List.assoc "l_orderkey" l) = Obj.obj (List.assoc "o_orderkey" o)) && (Obj.obj (List.assoc "n_nationkey" n) = Obj.obj (List.assoc "c_nationkey" c)) && (((((Obj.obj (List.assoc "o_orderdate" o) >= start_date) && Obj.obj (List.assoc "o_orderdate" o)) < end_date) && Obj.obj (List.assoc "l_returnflag" l)) = "R") then (
            let key = [("c_custkey",Obj.repr (Obj.obj (List.assoc "c_custkey" c)));("c_name",Obj.repr (Obj.obj (List.assoc "c_name" c)));("c_acctbal",Obj.repr (Obj.obj (List.assoc "c_acctbal" c)));("c_address",Obj.repr (Obj.obj (List.assoc "c_address" c)));("c_phone",Obj.repr (Obj.obj (List.assoc "c_phone" c)));("c_comment",Obj.repr (Obj.obj (List.assoc "c_comment" c)));("n_name",Obj.repr (Obj.obj (List.assoc "n_name" n)))] in
            let cur = try List.assoc key !__groups0 with Not_found -> [] in
            __groups0 := (key, c :: cur) :: List.remove_assoc key !__groups0);
                    ) nation;
            ) lineitem;
      ) orders;
  ) customer;
  let __res0 = ref [] in
  List.iter (fun (gKey,gItems) ->
    let g = { key = gKey; items = List.rev gItems } in
    __res0 := [("c_custkey",Obj.repr (g.key.c_custkey));("c_name",Obj.repr (g.key.c_name));("revenue",Obj.repr ((sum (let __res1 = ref [] in
  List.iter (fun x ->
      __res1 := (Obj.obj (List.assoc "l" x).l_extendedprice * ((1 - Obj.obj (List.assoc "l" x).l_discount))) :: !__res1;
  ) g.items;
List.rev !__res1)
)));("c_acctbal",Obj.repr (g.key.c_acctbal));("n_name",Obj.repr (g.key.n_name));("c_address",Obj.repr (g.key.c_address));("c_phone",Obj.repr (g.key.c_phone));("c_comment",Obj.repr (g.key.c_comment))] :: !__res0
  ) !__groups0;
  List.rev !__res0)


let () =
  print_endline (__show (result));
