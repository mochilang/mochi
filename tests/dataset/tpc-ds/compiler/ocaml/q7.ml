(* Generated by Mochi compiler v0.10.25 on 2025-07-15T04:50:15Z *)
type ('k,'v) group = { key : 'k; items : 'v list }

type record1 = { mutable ss_cdemo_sk : int; mutable ss_sold_date_sk : int; mutable ss_item_sk : int; mutable ss_promo_sk : int; mutable ss_quantity : int; mutable ss_list_price : float; mutable ss_coupon_amt : float; mutable ss_sales_price : float }
type record2 = { mutable cd_demo_sk : int; mutable cd_gender : string; mutable cd_marital_status : string; mutable cd_education_status : string }
type record3 = { mutable d_date_sk : int; mutable d_year : int }
type record4 = { mutable i_item_sk : int; mutable i_item_id : string }
type record5 = { mutable p_promo_sk : int; mutable p_channel_email : string; mutable p_channel_event : string }
type record6 = { mutable i_item_id : string }
type record7 = { mutable i_item_id : Obj.t; mutable agg1 : float; mutable agg2 : float; mutable agg3 : float; mutable agg4 : float }
type record8 = { mutable i_item_id : string; mutable agg1 : float; mutable agg2 : float; mutable agg3 : float; mutable agg4 : float }

let store_sales : record1 list = [{ ss_cdemo_sk = 1; ss_sold_date_sk = 1; ss_item_sk = 1; ss_promo_sk = 1; ss_quantity = 5; ss_list_price = 10.; ss_coupon_amt = 2.; ss_sales_price = 8. }]
let customer_demographics : record2 list = [{ cd_demo_sk = 1; cd_gender = "M"; cd_marital_status = "S"; cd_education_status = "College" }]
let date_dim : record3 list = [{ d_date_sk = 1; d_year = 1998 }]
let item : record4 list = [{ i_item_sk = 1; i_item_id = "I1" }]
let promotion : record5 list = [{ p_promo_sk = 1; p_channel_email = "N"; p_channel_event = "Y" }]
let result : record8 list = (let (__groups0 : (record6 * record1 list) list ref) = ref [] in
  List.iter (fun (ss : record1) ->
      List.iter (fun (cd : record2) ->
            List.iter (fun (d : record3) ->
                    List.iter (fun (i : record4) ->
                              List.iter (fun (p : record5) ->
                                                  if (ss.ss_cdemo_sk = cd.cd_demo_sk) && (ss.ss_sold_date_sk = d.d_date_sk) && (ss.ss_item_sk = i.i_item_sk) && (ss.ss_promo_sk = p.p_promo_sk) && (((((cd.cd_gender = "M") && (cd.cd_marital_status = "S")) && (cd.cd_education_status = "College")) && (((p.p_channel_email = "N") || (p.p_channel_event = "N")))) && (d.d_year = 1998)) then (
              let (key : record6) = [("i_item_id",Obj.repr (i.i_item_id))] in
              let cur = try List.assoc key !__groups0 with Not_found -> [] in
              __groups0 := (key, ss :: cur) :: List.remove_assoc key !__groups0);
                              ) promotion;
                    ) item;
            ) date_dim;
      ) customer_demographics;
  ) store_sales;
  let __res0 = ref [] in
  List.iter (fun ((gKey : record6), gItems) ->
    let g = { key = gKey; items = List.rev gItems } in
    __res0 := { i_item_id = g.key.i_item_id; agg1 = (float_of_int (List.fold_left (+) 0 (let __res1 = ref [] in
  List.iter (fun x ->
      __res1 := x.ss.ss_quantity :: !__res1;
  ) g.items;
List.rev !__res1)
) /. float_of_int (List.length (let __res1 = ref [] in
  List.iter (fun x ->
      __res1 := x.ss.ss_quantity :: !__res1;
  ) g.items;
List.rev !__res1)
)); agg2 = (float_of_int (List.fold_left (+) 0 (let __res2 = ref [] in
  List.iter (fun x ->
      __res2 := x.ss.ss_list_price :: !__res2;
  ) g.items;
List.rev !__res2)
) /. float_of_int (List.length (let __res2 = ref [] in
  List.iter (fun x ->
      __res2 := x.ss.ss_list_price :: !__res2;
  ) g.items;
List.rev !__res2)
)); agg3 = (float_of_int (List.fold_left (+) 0 (let __res3 = ref [] in
  List.iter (fun x ->
      __res3 := x.ss.ss_coupon_amt :: !__res3;
  ) g.items;
List.rev !__res3)
) /. float_of_int (List.length (let __res3 = ref [] in
  List.iter (fun x ->
      __res3 := x.ss.ss_coupon_amt :: !__res3;
  ) g.items;
List.rev !__res3)
)); agg4 = (float_of_int (List.fold_left (+) 0 (let __res4 = ref [] in
  List.iter (fun x ->
      __res4 := x.ss.ss_sales_price :: !__res4;
  ) g.items;
List.rev !__res4)
) /. float_of_int (List.length (let __res4 = ref [] in
  List.iter (fun x ->
      __res4 := x.ss.ss_sales_price :: !__res4;
  ) g.items;
List.rev !__res4)
)) } :: !__res0
  ) !__groups0;
  List.rev !__res0)


let () =
  json result;
  assert ((result = [{ i_item_id = "I1"; agg1 = 5.; agg2 = 10.; agg3 = 2.; agg4 = 8. }]))
