(* Generated by Mochi compiler v0.10.25 on 2025-07-15T04:50:17Z *)
let sum lst = List.fold_left (+) 0 lst
let sum_float lst = List.fold_left (+.) 0.0 lst
type ('k,'v) group = { key : 'k; items : 'v list }

type record1 = { mutable d_date_sk : int; mutable d_date : int }
type record2 = { mutable ss_sold_date_sk : int; mutable s_store_sk : int; mutable ss_ext_sales_price : float; mutable ss_net_profit : float }
type record3 = { mutable sr_returned_date_sk : int; mutable s_store_sk : int; mutable sr_return_amt : float; mutable sr_net_loss : float }
type record4 = { mutable cs_sold_date_sk : int; mutable cs_call_center_sk : int; mutable cs_ext_sales_price : float; mutable cs_net_profit : float }
type record5 = { mutable cr_returned_date_sk : int; mutable cr_call_center_sk : int; mutable cr_return_amount : float; mutable cr_net_loss : float }
type record6 = { mutable ws_sold_date_sk : int; mutable ws_web_page_sk : int; mutable ws_ext_sales_price : float; mutable ws_net_profit : float }
type record7 = { mutable wr_returned_date_sk : int; mutable wr_web_page_sk : int; mutable wr_return_amt : float; mutable wr_net_loss : float }
type record8 = { mutable s_store_sk : Obj.t; mutable sales : float; mutable profit : float }
type record9 = { mutable s_store_sk : int; mutable sales : float; mutable profit : float }
type record10 = { mutable s_store_sk : Obj.t; mutable returns : float; mutable profit_loss : float }
type record11 = { mutable s_store_sk : int; mutable returns : float; mutable profit_loss : float }
type record12 = { mutable cs_call_center_sk : Obj.t; mutable sales : float; mutable profit : float }
type record13 = { mutable cs_call_center_sk : int; mutable sales : float; mutable profit : float }
type record14 = { mutable cr_call_center_sk : Obj.t; mutable returns : float; mutable profit_loss : float }
type record15 = { mutable cr_call_center_sk : int; mutable returns : float; mutable profit_loss : float }
type record16 = { mutable wp_web_page_sk : Obj.t; mutable sales : float; mutable profit : float }
type record17 = { mutable wp_web_page_sk : int; mutable sales : float; mutable profit : float }
type record18 = { mutable wp_web_page_sk : Obj.t; mutable returns : float; mutable profit_loss : float }
type record19 = { mutable wp_web_page_sk : int; mutable returns : float; mutable profit_loss : float }
type record20 = { mutable channel : string; mutable id : int; mutable sales : float; mutable returns : float; mutable profit : float }
type record21 = { mutable channel : Obj.t; mutable id : Obj.t }
type record22 = { mutable channel : Obj.t; mutable id : Obj.t; mutable sales : float; mutable returns : float; mutable profit : float }

let date_dim : record1 list = [{ d_date_sk = 1; d_date = 1 }]
let store_sales : record2 list = [{ ss_sold_date_sk = 1; s_store_sk = 1; ss_ext_sales_price = 100.; ss_net_profit = 10. }]
let store_returns : record3 list = [{ sr_returned_date_sk = 1; s_store_sk = 1; sr_return_amt = 5.; sr_net_loss = 1. }]
let catalog_sales : record4 list = [{ cs_sold_date_sk = 1; cs_call_center_sk = 1; cs_ext_sales_price = 150.; cs_net_profit = 15. }]
let catalog_returns : record5 list = [{ cr_returned_date_sk = 1; cr_call_center_sk = 1; cr_return_amount = 7.; cr_net_loss = 3. }]
let web_sales : record6 list = [{ ws_sold_date_sk = 1; ws_web_page_sk = 1; ws_ext_sales_price = 200.; ws_net_profit = 20. }]
let web_returns : record7 list = [{ wr_returned_date_sk = 1; wr_web_page_sk = 1; wr_return_amt = 10.; wr_net_loss = 2. }]
let ss : record9 list = (let (__groups0 : (int * record2 list) list ref) = ref [] in
  List.iter (fun (ss : record2) ->
      List.iter (fun (d : record1) ->
              if (d.d_date_sk = ss.ss_sold_date_sk) then (
        let (key : int) = ss.s_store_sk in
        let cur = try List.assoc key !__groups0 with Not_found -> [] in
        __groups0 := (key, ss :: cur) :: List.remove_assoc key !__groups0);
      ) date_dim;
  ) store_sales;
  let __res0 = ref [] in
  List.iter (fun ((gKey : int), gItems) ->
    let g = { key = gKey; items = List.rev gItems } in
    __res0 := { s_store_sk = g.key; sales = (sum_float (let __res1 = ref [] in
  List.iter (fun x ->
      __res1 := x.ss.ss_ext_sales_price :: !__res1;
  ) g.items;
List.rev !__res1)
); profit = (sum_float (let __res2 = ref [] in
  List.iter (fun x ->
      __res2 := x.ss.ss_net_profit :: !__res2;
  ) g.items;
List.rev !__res2)
) } :: !__res0
  ) !__groups0;
  List.rev !__res0)

let sr : record11 list = (let (__groups3 : (int * record3 list) list ref) = ref [] in
  List.iter (fun (sr : record3) ->
      List.iter (fun (d : record1) ->
              if (d.d_date_sk = sr.sr_returned_date_sk) then (
        let (key : int) = sr.s_store_sk in
        let cur = try List.assoc key !__groups3 with Not_found -> [] in
        __groups3 := (key, sr :: cur) :: List.remove_assoc key !__groups3);
      ) date_dim;
  ) store_returns;
  let __res3 = ref [] in
  List.iter (fun ((gKey : int), gItems) ->
    let g = { key = gKey; items = List.rev gItems } in
    __res3 := { s_store_sk = g.key; returns = (sum_float (let __res4 = ref [] in
  List.iter (fun x ->
      __res4 := x.sr.sr_return_amt :: !__res4;
  ) g.items;
List.rev !__res4)
); profit_loss = (sum_float (let __res5 = ref [] in
  List.iter (fun x ->
      __res5 := x.sr.sr_net_loss :: !__res5;
  ) g.items;
List.rev !__res5)
) } :: !__res3
  ) !__groups3;
  List.rev !__res3)

let cs : record13 list = (let (__groups6 : (int * record4 list) list ref) = ref [] in
  List.iter (fun (cs : record4) ->
      List.iter (fun (d : record1) ->
              if (d.d_date_sk = cs.cs_sold_date_sk) then (
        let (key : int) = cs.cs_call_center_sk in
        let cur = try List.assoc key !__groups6 with Not_found -> [] in
        __groups6 := (key, cs :: cur) :: List.remove_assoc key !__groups6);
      ) date_dim;
  ) catalog_sales;
  let __res6 = ref [] in
  List.iter (fun ((gKey : int), gItems) ->
    let g = { key = gKey; items = List.rev gItems } in
    __res6 := { cs_call_center_sk = g.key; sales = (sum_float (let __res7 = ref [] in
  List.iter (fun x ->
      __res7 := x.cs.cs_ext_sales_price :: !__res7;
  ) g.items;
List.rev !__res7)
); profit = (sum_float (let __res8 = ref [] in
  List.iter (fun x ->
      __res8 := x.cs.cs_net_profit :: !__res8;
  ) g.items;
List.rev !__res8)
) } :: !__res6
  ) !__groups6;
  List.rev !__res6)

let cr : record15 list = (let (__groups9 : (int * record5 list) list ref) = ref [] in
  List.iter (fun (cr : record5) ->
      List.iter (fun (d : record1) ->
              if (d.d_date_sk = cr.cr_returned_date_sk) then (
        let (key : int) = cr.cr_call_center_sk in
        let cur = try List.assoc key !__groups9 with Not_found -> [] in
        __groups9 := (key, cr :: cur) :: List.remove_assoc key !__groups9);
      ) date_dim;
  ) catalog_returns;
  let __res9 = ref [] in
  List.iter (fun ((gKey : int), gItems) ->
    let g = { key = gKey; items = List.rev gItems } in
    __res9 := { cr_call_center_sk = g.key; returns = (sum_float (let __res10 = ref [] in
  List.iter (fun x ->
      __res10 := x.cr.cr_return_amount :: !__res10;
  ) g.items;
List.rev !__res10)
); profit_loss = (sum_float (let __res11 = ref [] in
  List.iter (fun x ->
      __res11 := x.cr.cr_net_loss :: !__res11;
  ) g.items;
List.rev !__res11)
) } :: !__res9
  ) !__groups9;
  List.rev !__res9)

let ws : record17 list = (let (__groups12 : (int * record6 list) list ref) = ref [] in
  List.iter (fun (ws : record6) ->
      List.iter (fun (d : record1) ->
              if (d.d_date_sk = ws.ws_sold_date_sk) then (
        let (key : int) = ws.ws_web_page_sk in
        let cur = try List.assoc key !__groups12 with Not_found -> [] in
        __groups12 := (key, ws :: cur) :: List.remove_assoc key !__groups12);
      ) date_dim;
  ) web_sales;
  let __res12 = ref [] in
  List.iter (fun ((gKey : int), gItems) ->
    let g = { key = gKey; items = List.rev gItems } in
    __res12 := { wp_web_page_sk = g.key; sales = (sum_float (let __res13 = ref [] in
  List.iter (fun x ->
      __res13 := x.ws.ws_ext_sales_price :: !__res13;
  ) g.items;
List.rev !__res13)
); profit = (sum_float (let __res14 = ref [] in
  List.iter (fun x ->
      __res14 := x.ws.ws_net_profit :: !__res14;
  ) g.items;
List.rev !__res14)
) } :: !__res12
  ) !__groups12;
  List.rev !__res12)

let wr : record19 list = (let (__groups15 : (int * record7 list) list ref) = ref [] in
  List.iter (fun (wr : record7) ->
      List.iter (fun (d : record1) ->
              if (d.d_date_sk = wr.wr_returned_date_sk) then (
        let (key : int) = wr.wr_web_page_sk in
        let cur = try List.assoc key !__groups15 with Not_found -> [] in
        __groups15 := (key, wr :: cur) :: List.remove_assoc key !__groups15);
      ) date_dim;
  ) web_returns;
  let __res15 = ref [] in
  List.iter (fun ((gKey : int), gItems) ->
    let g = { key = gKey; items = List.rev gItems } in
    __res15 := { wp_web_page_sk = g.key; returns = (sum_float (let __res16 = ref [] in
  List.iter (fun x ->
      __res16 := x.wr.wr_return_amt :: !__res16;
  ) g.items;
List.rev !__res16)
); profit_loss = (sum_float (let __res17 = ref [] in
  List.iter (fun x ->
      __res17 := x.wr.wr_net_loss :: !__res17;
  ) g.items;
List.rev !__res17)
) } :: !__res15
  ) !__groups15;
  List.rev !__res15)

let per_channel : Obj.t list = concat (let __res18 = ref [] in
  List.iter (fun (s : record9) ->
    let matched = ref false in
    List.iter (fun (r : record11) ->
      if (s.s_store_sk = r.s_store_sk) then (
        __res18 := { channel = "store channel"; id = s.s_store_sk; sales = s.sales; returns = (if (r = ()) then 0. else r.returns); profit = (s.profit -. ((if (r = ()) then 0. else r.profit_loss))) } :: !__res18;
        matched := true)
    ) sr;
    if not !matched then (
      let r = Obj.magic () in
      __res18 := { channel = "store channel"; id = s.s_store_sk; sales = s.sales; returns = (if (r = ()) then 0. else r.returns); profit = (s.profit -. ((if (r = ()) then 0. else r.profit_loss))) } :: !__res18;
    );
  ) ss;
  List.rev !__res18)
 (let __res19 = ref [] in
  List.iter (fun c ->
    List.iter (fun r ->
      if (c.cs_call_center_sk = r.cr_call_center_sk) then (
        __res19 := { channel = "catalog channel"; id = c.cs_call_center_sk; sales = c.sales; returns = r.returns; profit = (c.profit -. r.profit_loss) } :: !__res19;
      )
    ) cr;
  ) cs;
  List.rev !__res19)
 (let __res20 = ref [] in
  List.iter (fun (w : record17) ->
    let matched = ref false in
    List.iter (fun (r : record19) ->
      if (w.wp_web_page_sk = r.wp_web_page_sk) then (
        __res20 := { channel = "web channel"; id = w.wp_web_page_sk; sales = w.sales; returns = (if (r = ()) then 0. else r.returns); profit = (w.profit -. ((if (r = ()) then 0. else r.profit_loss))) } :: !__res20;
        matched := true)
    ) wr;
    if not !matched then (
      let r = Obj.magic () in
      __res20 := { channel = "web channel"; id = w.wp_web_page_sk; sales = w.sales; returns = (if (r = ()) then 0. else r.returns); profit = (w.profit -. ((if (r = ()) then 0. else r.profit_loss))) } :: !__res20;
    );
  ) ws;
  List.rev !__res20)

let result : record22 list = (let (__groups21 : (record21 * Obj.t list) list ref) = ref [] in
  List.iter (fun p ->
      let (key : record21) = { channel = p.channel; id = p.id } in
      let cur = try List.assoc key !__groups21 with Not_found -> [] in
      __groups21 := (key, p :: cur) :: List.remove_assoc key !__groups21;
  ) per_channel;
  let __res21 = ref [] in
  List.iter (fun ((gKey : record21), gItems) ->
    let g = { key = gKey; items = List.rev gItems } in
    __res21 := { channel = g.key.channel; id = g.key.id; sales = (sum_float (let __res22 = ref [] in
  List.iter (fun x ->
      __res22 := x.p.sales :: !__res22;
  ) g.items;
List.rev !__res22)
); returns = (sum_float (let __res23 = ref [] in
  List.iter (fun x ->
      __res23 := x.p.returns :: !__res23;
  ) g.items;
List.rev !__res23)
); profit = (sum_float (let __res24 = ref [] in
  List.iter (fun x ->
      __res24 := x.p.profit :: !__res24;
  ) g.items;
List.rev !__res24)
) } :: !__res21
  ) !__groups21;
  List.rev !__res21)


let () =
  json result;
  assert ((result = [{ channel = "catalog channel"; id = 1; sales = 150.; returns = 7.; profit = 12. };{ channel = "store channel"; id = 1; sales = 100.; returns = 5.; profit = 9. };{ channel = "web channel"; id = 1; sales = 200.; returns = 10.; profit = 18. }]))
