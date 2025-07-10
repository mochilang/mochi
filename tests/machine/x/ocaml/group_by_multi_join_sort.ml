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

type record1 = { mutable n_nationkey : int; mutable n_name : string }
type record2 = { mutable c_custkey : int; mutable c_name : string; mutable c_acctbal : float; mutable c_nationkey : int; mutable c_address : string; mutable c_phone : string; mutable c_comment : string }
type record3 = { mutable o_orderkey : int; mutable o_custkey : int; mutable o_orderdate : string }
type record4 = { mutable l_orderkey : int; mutable l_returnflag : string; mutable l_extendedprice : float; mutable l_discount : float }
type record5 = { mutable c_custkey : Obj.t; mutable c_name : Obj.t; mutable c_acctbal : Obj.t; mutable c_address : Obj.t; mutable c_phone : Obj.t; mutable c_comment : Obj.t; mutable n_name : Obj.t }
type record6 = { mutable c_custkey : Obj.t; mutable c_name : Obj.t; mutable revenue : float; mutable c_acctbal : Obj.t; mutable n_name : Obj.t; mutable c_address : Obj.t; mutable c_phone : Obj.t; mutable c_comment : Obj.t }

let nation : record1 list = [{ n_nationkey = 1; n_name = "BRAZIL" }]
let customer : record2 list = [{ c_custkey = 1; c_name = "Alice"; c_acctbal = 100; c_nationkey = 1; c_address = "123 St"; c_phone = "123-456"; c_comment = "Loyal" }]
let orders : record3 list = [{ o_orderkey = 1000; o_custkey = 1; o_orderdate = "1993-10-15" };{ o_orderkey = 2000; o_custkey = 1; o_orderdate = "1994-01-02" }]
let lineitem : record4 list = [{ l_orderkey = 1000; l_returnflag = "R"; l_extendedprice = 1000; l_discount = 0.1 };{ l_orderkey = 2000; l_returnflag = "N"; l_extendedprice = 500; l_discount = 0 }]
let start_date : string = "1993-10-01"
let end_date : string = "1994-01-01"
let result : (string * Obj.t) list list = (let __groups0 = ref [] in
  List.iter (fun c ->
      List.iter (fun o ->
            List.iter (fun l ->
                    List.iter (fun n ->
                                    if (Obj.obj (List.assoc "o_custkey" o) = Obj.obj (List.assoc "c_custkey" c)) && (Obj.obj (List.assoc "l_orderkey" l) = Obj.obj (List.assoc "o_orderkey" o)) && (Obj.obj (List.assoc "n_nationkey" n) = Obj.obj (List.assoc "c_nationkey" c)) && (((((Obj.obj (List.assoc "o_orderdate" o) >= start_date) && Obj.obj (List.assoc "o_orderdate" o)) < end_date) && Obj.obj (List.assoc "l_returnflag" l)) = "R") then (
            let key = { c_custkey = Obj.obj (List.assoc "c_custkey" c); c_name = Obj.obj (List.assoc "c_name" c); c_acctbal = Obj.obj (List.assoc "c_acctbal" c); c_address = Obj.obj (List.assoc "c_address" c); c_phone = Obj.obj (List.assoc "c_phone" c); c_comment = Obj.obj (List.assoc "c_comment" c); n_name = Obj.obj (List.assoc "n_name" n) } in
            let cur = try List.assoc key !__groups0 with Not_found -> [] in
            __groups0 := (key, c :: cur) :: List.remove_assoc key !__groups0);
                    ) nation;
            ) lineitem;
      ) orders;
  ) customer;
  let __res0 = ref [] in
  List.iter (fun (gKey,gItems) ->
    let g = { key = gKey; items = List.rev gItems } in
    __res0 := { c_custkey = g.key.c_custkey; c_name = g.key.c_name; revenue = (sum (let __res1 = ref [] in
  List.iter (fun x ->
      __res1 := (Obj.obj (List.assoc "l" x).l_extendedprice * ((1 - Obj.obj (List.assoc "l" x).l_discount))) :: !__res1;
  ) g.items;
List.rev !__res1)
); c_acctbal = g.key.c_acctbal; n_name = g.key.n_name; c_address = g.key.c_address; c_phone = g.key.c_phone; c_comment = g.key.c_comment } :: !__res0
  ) !__groups0;
  List.rev !__res0)


let () =
  print_endline (__show (result));
