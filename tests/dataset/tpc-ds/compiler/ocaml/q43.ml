(* Generated by Mochi compiler v0.10.25 on 2025-07-15T04:50:17Z *)
let sum lst = List.fold_left (+) 0 lst
let sum_float lst = List.fold_left (+.) 0.0 lst
type ('k,'v) group = { key : 'k; items : 'v list }

type record1 = { mutable date_sk : int; mutable d_day_name : string; mutable d_year : int }
type record2 = { mutable store_sk : int; mutable store_id : string; mutable store_name : string; mutable gmt_offset : int }
type record3 = { mutable sold_date_sk : int; mutable store_sk : int; mutable sales_price : float }
type record4 = { mutable d_day_name : string; mutable s_store_name : string; mutable s_store_id : string; mutable price : float }
type record5 = { mutable name : string; mutable id : string }
type record6 = { mutable s_store_name : Obj.t; mutable s_store_id : Obj.t; mutable sun_sales : float; mutable mon_sales : float; mutable tue_sales : float; mutable wed_sales : float; mutable thu_sales : float; mutable fri_sales : float; mutable sat_sales : float }
type record7 = { mutable s_store_name : string; mutable s_store_id : string; mutable sun_sales : float; mutable mon_sales : float; mutable tue_sales : float; mutable wed_sales : float; mutable thu_sales : float; mutable fri_sales : float; mutable sat_sales : float }

let date_dim : record1 list = [{ date_sk = 1; d_day_name = "Sunday"; d_year = 2020 };{ date_sk = 2; d_day_name = "Monday"; d_year = 2020 };{ date_sk = 3; d_day_name = "Tuesday"; d_year = 2020 };{ date_sk = 4; d_day_name = "Wednesday"; d_year = 2020 };{ date_sk = 5; d_day_name = "Thursday"; d_year = 2020 };{ date_sk = 6; d_day_name = "Friday"; d_year = 2020 };{ date_sk = 7; d_day_name = "Saturday"; d_year = 2020 }]
let store : record2 list = [{ store_sk = 1; store_id = "S1"; store_name = "Main"; gmt_offset = 0 }]
let store_sales : record3 list = [{ sold_date_sk = 1; store_sk = 1; sales_price = 10. };{ sold_date_sk = 2; store_sk = 1; sales_price = 20. };{ sold_date_sk = 3; store_sk = 1; sales_price = 30. };{ sold_date_sk = 4; store_sk = 1; sales_price = 40. };{ sold_date_sk = 5; store_sk = 1; sales_price = 50. };{ sold_date_sk = 6; store_sk = 1; sales_price = 60. };{ sold_date_sk = 7; store_sk = 1; sales_price = 70. }]
let year : int = 2020
let gmt : int = 0
let records : record4 list = (let __res0 = ref [] in
  List.iter (fun (d : record1) ->
      List.iter (fun (ss : record3) ->
            List.iter (fun (s : record2) ->
                        if (ss.sold_date_sk = d.date_sk) && (ss.store_sk = s.store_sk) && ((s.gmt_offset = gmt) && (d.d_year = year)) then
        __res0 := { d_day_name = d.d_day_name; s_store_name = s.store_name; s_store_id = s.store_id; price = ss.sales_price } :: !__res0;
            ) store;
      ) store_sales;
  ) date_dim;
List.rev !__res0)

let base : record7 list = (let (__groups1 : (record5 * record4 list) list ref) = ref [] in
  List.iter (fun (r : record4) ->
      let (key : record5) = { name = r.s_store_name; id = r.s_store_id } in
      let cur = try List.assoc key !__groups1 with Not_found -> [] in
      __groups1 := (key, r :: cur) :: List.remove_assoc key !__groups1;
  ) records;
  let __res1 = ref [] in
  List.iter (fun ((gKey : record5), gItems) ->
    let g = { key = gKey; items = List.rev gItems } in
    __res1 := { s_store_name = g.key.name; s_store_id = g.key.id; sun_sales = (sum_float (let __res2 = ref [] in
  List.iter (fun x ->
      __res2 := (if (x.d_day_name = "Sunday") then x.price else 0.) :: !__res2;
  ) g.items;
List.rev !__res2)
); mon_sales = (sum_float (let __res3 = ref [] in
  List.iter (fun x ->
      __res3 := (if (x.d_day_name = "Monday") then x.price else 0.) :: !__res3;
  ) g.items;
List.rev !__res3)
); tue_sales = (sum_float (let __res4 = ref [] in
  List.iter (fun x ->
      __res4 := (if (x.d_day_name = "Tuesday") then x.price else 0.) :: !__res4;
  ) g.items;
List.rev !__res4)
); wed_sales = (sum_float (let __res5 = ref [] in
  List.iter (fun x ->
      __res5 := (if (x.d_day_name = "Wednesday") then x.price else 0.) :: !__res5;
  ) g.items;
List.rev !__res5)
); thu_sales = (sum_float (let __res6 = ref [] in
  List.iter (fun x ->
      __res6 := (if (x.d_day_name = "Thursday") then x.price else 0.) :: !__res6;
  ) g.items;
List.rev !__res6)
); fri_sales = (sum_float (let __res7 = ref [] in
  List.iter (fun x ->
      __res7 := (if (x.d_day_name = "Friday") then x.price else 0.) :: !__res7;
  ) g.items;
List.rev !__res7)
); sat_sales = (sum_float (let __res8 = ref [] in
  List.iter (fun x ->
      __res8 := (if (x.d_day_name = "Saturday") then x.price else 0.) :: !__res8;
  ) g.items;
List.rev !__res8)
) } :: !__res1
  ) !__groups1;
  List.rev !__res1)

let result : (string * Obj.t) list list = base

let () =
  json result;
  assert ((result = [{ s_store_name = "Main"; s_store_id = "S1"; sun_sales = 10.; mon_sales = 20.; tue_sales = 30.; wed_sales = 40.; thu_sales = 50.; fri_sales = 60.; sat_sales = 70. }]))
