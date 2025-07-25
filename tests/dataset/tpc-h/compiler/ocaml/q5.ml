(* Generated by Mochi compiler v0.10.28 on 2025-07-18T03:21:55Z *)
let sum lst = List.fold_left (+) 0 lst
let sum_float lst = List.fold_left (+.) 0.0 lst
type ('k,'v) group = { key : 'k; items : 'v list }
let rec __to_json v =
  let open Obj in
  let rec list_aux o =
    if is_int o && (magic (obj o) : int) = 0 then "" else
     let hd = field o 0 in
     let tl = field o 1 in
     let rest = list_aux tl in
     let cur = __to_json (obj hd) in
     if rest = "" then cur else cur ^ "," ^ rest
  in
  let r = repr v in
  if is_int r then string_of_int (magic v) else
  match tag r with
    | 0 -> if size r = 0 then "[]" else "[" ^ list_aux r ^ "]"
    | 252 -> Printf.sprintf "%S" (magic v : string)
    | 253 -> string_of_float (magic v)
    | _ -> "null"

let _json v = print_endline (__to_json v)


type record1 = { mutable r_regionkey : int; mutable r_name : string }
type record2 = { mutable n_nationkey : int; mutable n_regionkey : int; mutable n_name : string }
type record3 = { mutable c_custkey : int; mutable c_nationkey : int }
type record4 = { mutable s_suppkey : int; mutable s_nationkey : int }
type record5 = { mutable o_orderkey : int; mutable o_custkey : int; mutable o_orderdate : string }
type record6 = { mutable l_orderkey : int; mutable l_suppkey : int; mutable l_extendedprice : float; mutable l_discount : float }
type record7 = { mutable nation : string; mutable revenue : float }
type record8 = { mutable n_name : Obj.t; mutable revenue : float }
type record9 = { mutable n_name : string; mutable revenue : float }
type record10 = { mutable n_name : string; mutable revenue : int }

let region : record1 list = [{ r_regionkey = 0; r_name = "ASIA" };{ r_regionkey = 1; r_name = "EUROPE" }]
let nation : record2 list = [{ n_nationkey = 10; n_regionkey = 0; n_name = "JAPAN" };{ n_nationkey = 20; n_regionkey = 0; n_name = "INDIA" };{ n_nationkey = 30; n_regionkey = 1; n_name = "FRANCE" }]
let customer : record3 list = [{ c_custkey = 1; c_nationkey = 10 };{ c_custkey = 2; c_nationkey = 20 }]
let supplier : record4 list = [{ s_suppkey = 100; s_nationkey = 10 };{ s_suppkey = 200; s_nationkey = 20 }]
let orders : record5 list = [{ o_orderkey = 1000; o_custkey = 1; o_orderdate = "1994-03-15" };{ o_orderkey = 2000; o_custkey = 2; o_orderdate = "1994-06-10" };{ o_orderkey = 3000; o_custkey = 2; o_orderdate = "1995-01-01" }]
let lineitem : record6 list = [{ l_orderkey = 1000; l_suppkey = 100; l_extendedprice = 1000.; l_discount = 0.05 };{ l_orderkey = 2000; l_suppkey = 200; l_extendedprice = 800.; l_discount = 0.1 };{ l_orderkey = 3000; l_suppkey = 200; l_extendedprice = 900.; l_discount = 0.05 }]
let asia_nations : record2 list = (let __res0 = ref [] in
  List.iter (fun (r : record1) ->
    List.iter (fun (n : record2) ->
      if (n.n_regionkey = r.r_regionkey) then (
        if (r.r_name = "ASIA") then __res0 := n :: !__res0;
      )
    ) nation;
  ) region;
  List.rev !__res0)

let local_customer_supplier_orders : record7 list = (let __res1 = ref [] in
  List.iter (fun (c : record3) ->
      List.iter (fun (n : record2) ->
            List.iter (fun (o : record5) ->
                    List.iter (fun (l : record6) ->
                              List.iter (fun (s : record4) ->
                                                  if (c.c_nationkey = n.n_nationkey) && (o.o_custkey = c.c_custkey) && (l.l_orderkey = o.o_orderkey) && (s.s_suppkey = l.l_suppkey) && (((o.o_orderdate >= "1994-01-01") && (o.o_orderdate < "1995-01-01")) && (s.s_nationkey = c.c_nationkey)) then
            __res1 := { nation = n.n_name; revenue = (l.l_extendedprice *. ((1 -. l.l_discount))) } :: !__res1;
                              ) supplier;
                    ) lineitem;
            ) orders;
      ) asia_nations;
  ) customer;
List.rev !__res1)

let result : record9 list = (let (__groups2 : (string * record7 list) list ref) = ref [] in
  List.iter (fun (r : record7) ->
      let (key : string) = r.nation in
      let cur = try List.assoc key !__groups2 with Not_found -> [] in
      __groups2 := (key, r :: cur) :: List.remove_assoc key !__groups2;
  ) local_customer_supplier_orders;
  let __res2 = ref [] in
  List.iter (fun ((gKey : string), gItems) ->
    let g = { key = gKey; items = List.rev gItems } in
    __res2 := { n_name = g.key; revenue = (sum_float (let __res3 = ref [] in
  List.iter (fun x ->
      __res3 := x.revenue :: !__res3;
  ) g.items;
List.rev !__res3)
) } :: !__res2
  ) !__groups2;
  List.rev !__res2)


let () =
  _json result;
  assert ((result = [{ n_name = "JAPAN"; revenue = 950 };{ n_name = "INDIA"; revenue = 720 }]))
