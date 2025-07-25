(* Generated by Mochi compiler v0.10.25 on 2025-07-15T04:50:16Z *)
type ('k,'v) group = { key : 'k; items : 'v list }

type record1 = { mutable c_customer_sk : int; mutable c_current_addr_sk : int; mutable c_current_cdemo_sk : int }
type record2 = { mutable ca_address_sk : int; mutable ca_county : string }
type record3 = { mutable cd_demo_sk : int; mutable cd_gender : string; mutable cd_marital_status : string; mutable cd_education_status : string; mutable cd_purchase_estimate : int; mutable cd_credit_rating : string; mutable cd_dep_count : int; mutable cd_dep_employed_count : int; mutable cd_dep_college_count : int }
type record4 = { mutable ss_customer_sk : int; mutable ss_sold_date_sk : int }
type record5 = { mutable d_date_sk : int; mutable d_year : int; mutable d_moy : int }
type record6 = { mutable gender : string; mutable marital : string; mutable education : string; mutable purchase : int; mutable credit : string; mutable dep : int; mutable depemp : int; mutable depcol : int }
type record7 = { mutable cd_gender : Obj.t; mutable cd_marital_status : Obj.t; mutable cd_education_status : Obj.t; mutable cnt1 : int; mutable cd_purchase_estimate : Obj.t; mutable cnt2 : int; mutable cd_credit_rating : Obj.t; mutable cnt3 : int; mutable cd_dep_count : Obj.t; mutable cnt4 : int; mutable cd_dep_employed_count : Obj.t; mutable cnt5 : int; mutable cd_dep_college_count : Obj.t; mutable cnt6 : int }
type record8 = { mutable cd_gender : string; mutable cd_marital_status : string; mutable cd_education_status : string; mutable cnt1 : int; mutable cd_purchase_estimate : int; mutable cnt2 : int; mutable cd_credit_rating : string; mutable cnt3 : int; mutable cd_dep_count : int; mutable cnt4 : int; mutable cd_dep_employed_count : int; mutable cnt5 : int; mutable cd_dep_college_count : int; mutable cnt6 : int }

type customer = { mutable c_customer_sk : int; mutable c_current_addr_sk : int; mutable c_current_cdemo_sk : int }
type customeraddress = { mutable ca_address_sk : int; mutable ca_county : string }
type customerdemographics = { mutable cd_demo_sk : int; mutable cd_gender : string; mutable cd_marital_status : string; mutable cd_education_status : string; mutable cd_purchase_estimate : int; mutable cd_credit_rating : string; mutable cd_dep_count : int; mutable cd_dep_employed_count : int; mutable cd_dep_college_count : int }
type storesale = { mutable ss_customer_sk : int; mutable ss_sold_date_sk : int }
type datedim = { mutable d_date_sk : int; mutable d_year : int; mutable d_moy : int }
let customer : record1 list = [{ c_customer_sk = 1; c_current_addr_sk = 1; c_current_cdemo_sk = 1 }]
let customer_address : record2 list = [{ ca_address_sk = 1; ca_county = "CountyA" }]
let customer_demographics : record3 list = [{ cd_demo_sk = 1; cd_gender = "F"; cd_marital_status = "M"; cd_education_status = "College"; cd_purchase_estimate = 5000; cd_credit_rating = "Good"; cd_dep_count = 1; cd_dep_employed_count = 1; cd_dep_college_count = 0 }]
let store_sales : record4 list = [{ ss_customer_sk = 1; ss_sold_date_sk = 1 }]
let web_sales : Obj.t list = []
let catalog_sales : Obj.t list = []
let date_dim : record5 list = [{ d_date_sk = 1; d_year = 2000; d_moy = 2 }]
let active : record3 list = (let __res1 = ref [] in
  List.iter (fun (c : record1) ->
      List.iter (fun (ca : record2) ->
            List.iter (fun (cd : record3) ->
                        if ((c.c_current_addr_sk = ca.ca_address_sk) && (ca.ca_county = "CountyA")) && (c.c_current_cdemo_sk = cd.cd_demo_sk) && ((let __res0 = ref [] in
  List.iter (fun ss ->
    List.iter (fun d ->
      if (ss.ss_sold_date_sk = d.d_date_sk) then (
        if ((((ss.ss_customer_sk = c.c_customer_sk) && (d.d_year = 2000)) && (d.d_moy >= 2)) && (d.d_moy <= 5)) then __res0 := ss :: !__res0;
      )
    ) date_dim;
  ) store_sales;
  List.rev !__res0)
 <> []) then
        __res1 := cd :: !__res1;
            ) customer_demographics;
      ) customer_address;
  ) customer;
List.rev !__res1)

let result : record8 list = (let (__groups2 : (record6 * record3 list) list ref) = ref [] in
  List.iter (fun (a : record3) ->
      let (key : record6) = { gender = a.cd_gender; marital = a.cd_marital_status; education = a.cd_education_status; purchase = a.cd_purchase_estimate; credit = a.cd_credit_rating; dep = a.cd_dep_count; depemp = a.cd_dep_employed_count; depcol = a.cd_dep_college_count } in
      let cur = try List.assoc key !__groups2 with Not_found -> [] in
      __groups2 := (key, a :: cur) :: List.remove_assoc key !__groups2;
  ) active;
  let __res2 = ref [] in
  List.iter (fun ((gKey : record6), gItems) ->
    let g = { key = gKey; items = List.rev gItems } in
    __res2 := { cd_gender = g.key.gender; cd_marital_status = g.key.marital; cd_education_status = g.key.education; cnt1 = List.length (let __res3 = ref [] in
  List.iter (fun _ ->
      __res3 := _ :: !__res3;
  ) g.items;
List.rev !__res3)
; cd_purchase_estimate = g.key.purchase; cnt2 = List.length (let __res4 = ref [] in
  List.iter (fun _ ->
      __res4 := _ :: !__res4;
  ) g.items;
List.rev !__res4)
; cd_credit_rating = g.key.credit; cnt3 = List.length (let __res5 = ref [] in
  List.iter (fun _ ->
      __res5 := _ :: !__res5;
  ) g.items;
List.rev !__res5)
; cd_dep_count = g.key.dep; cnt4 = List.length (let __res6 = ref [] in
  List.iter (fun _ ->
      __res6 := _ :: !__res6;
  ) g.items;
List.rev !__res6)
; cd_dep_employed_count = g.key.depemp; cnt5 = List.length (let __res7 = ref [] in
  List.iter (fun _ ->
      __res7 := _ :: !__res7;
  ) g.items;
List.rev !__res7)
; cd_dep_college_count = g.key.depcol; cnt6 = List.length (let __res8 = ref [] in
  List.iter (fun _ ->
      __res8 := _ :: !__res8;
  ) g.items;
List.rev !__res8)
 } :: !__res2
  ) !__groups2;
  List.rev !__res2)


let () =
  json result;
  assert ((result = [{ cd_gender = "F"; cd_marital_status = "M"; cd_education_status = "College"; cnt1 = 1; cd_purchase_estimate = 5000; cnt2 = 1; cd_credit_rating = "Good"; cnt3 = 1; cd_dep_count = 1; cnt4 = 1; cd_dep_employed_count = 1; cnt5 = 1; cd_dep_college_count = 0; cnt6 = 1 }]))
