(* Generated by Mochi compiler v0.10.25 on 2025-07-15T04:50:16Z *)
  exception Break
  exception Continue

  let sum lst = List.fold_left (+) 0 lst
  let sum_float lst = List.fold_left (+.) 0.0 lst

  type record1 = { mutable ca_county : string; mutable d_qoy : int; mutable d_year : int; mutable ss_ext_sales_price : float }
  type record2 = { mutable ca_county : string; mutable d_qoy : int; mutable d_year : int; mutable ws_ext_sales_price : float }
  type record3 = { mutable ca_county : string; mutable d_year : int; mutable web_q1_q2_increase : Obj.t; mutable store_q1_q2_increase : Obj.t; mutable web_q2_q3_increase : Obj.t; mutable store_q2_q3_increase : Obj.t }
  type record4 = { mutable ca_county : string; mutable d_year : int; mutable web_q1_q2_increase : float; mutable store_q1_q2_increase : float; mutable web_q2_q3_increase : float; mutable store_q2_q3_increase : float }

let store_sales : record1 list = [{ ca_county = "A"; d_qoy = 1; d_year = 2000; ss_ext_sales_price = 100. };{ ca_county = "A"; d_qoy = 2; d_year = 2000; ss_ext_sales_price = 120. };{ ca_county = "A"; d_qoy = 3; d_year = 2000; ss_ext_sales_price = 160. };{ ca_county = "B"; d_qoy = 1; d_year = 2000; ss_ext_sales_price = 80. };{ ca_county = "B"; d_qoy = 2; d_year = 2000; ss_ext_sales_price = 90. };{ ca_county = "B"; d_qoy = 3; d_year = 2000; ss_ext_sales_price = 100. }]
let web_sales : record2 list = [{ ca_county = "A"; d_qoy = 1; d_year = 2000; ws_ext_sales_price = 100. };{ ca_county = "A"; d_qoy = 2; d_year = 2000; ws_ext_sales_price = 150. };{ ca_county = "A"; d_qoy = 3; d_year = 2000; ws_ext_sales_price = 250. };{ ca_county = "B"; d_qoy = 1; d_year = 2000; ws_ext_sales_price = 80. };{ ca_county = "B"; d_qoy = 2; d_year = 2000; ws_ext_sales_price = 90. };{ ca_county = "B"; d_qoy = 3; d_year = 2000; ws_ext_sales_price = 95. }]
let counties : string list = ["A";"B"]
let result : Obj.t list ref = ref []

let () =
  let rec __loop0 lst =
    match lst with
      | [] -> ()
      | county::rest ->
        (try
          let ss1 : float = (sum_float (let __res1 = ref [] in
  List.iter (fun (s : record1) ->
      if ((s.ca_county = county) && (s.d_qoy = 1)) then
    __res1 := s.ss_ext_sales_price :: !__res1;
  ) store_sales;
List.rev !__res1)
) in
          let ss2 : float = (sum_float (let __res2 = ref [] in
  List.iter (fun (s : record1) ->
      if ((s.ca_county = county) && (s.d_qoy = 2)) then
    __res2 := s.ss_ext_sales_price :: !__res2;
  ) store_sales;
List.rev !__res2)
) in
          let ss3 : float = (sum_float (let __res3 = ref [] in
  List.iter (fun (s : record1) ->
      if ((s.ca_county = county) && (s.d_qoy = 3)) then
    __res3 := s.ss_ext_sales_price :: !__res3;
  ) store_sales;
List.rev !__res3)
) in
          let ws1 : float = (sum_float (let __res4 = ref [] in
  List.iter (fun (w : record2) ->
      if ((w.ca_county = county) && (w.d_qoy = 1)) then
    __res4 := w.ws_ext_sales_price :: !__res4;
  ) web_sales;
List.rev !__res4)
) in
          let ws2 : float = (sum_float (let __res5 = ref [] in
  List.iter (fun (w : record2) ->
      if ((w.ca_county = county) && (w.d_qoy = 2)) then
    __res5 := w.ws_ext_sales_price :: !__res5;
  ) web_sales;
List.rev !__res5)
) in
          let ws3 : float = (sum_float (let __res6 = ref [] in
  List.iter (fun (w : record2) ->
      if ((w.ca_county = county) && (w.d_qoy = 3)) then
    __res6 := w.ws_ext_sales_price :: !__res6;
  ) web_sales;
List.rev !__res6)
) in
          let web_g1 = (ws2 / ws1) in
          let store_g1 = (ss2 / ss1) in
          let web_g2 = (ws3 / ws2) in
          let store_g2 = (ss3 / ss2) in
          if ((web_g1 > store_g1) && (web_g2 > store_g2)) then (
            result := ((!result) @ [{ ca_county = county; d_year = 2000; web_q1_q2_increase = web_g1; store_q1_q2_increase = store_g1; web_q2_q3_increase = web_g2; store_q2_q3_increase = store_g2 }]);
          ) ;
        with Continue -> ())
        ; __loop0 rest
    in
    try __loop0 counties with Break -> ()
    json (!result);
    assert (((!result) = [{ ca_county = "A"; d_year = 2000; web_q1_q2_increase = 1.5; store_q1_q2_increase = 1.2; web_q2_q3_increase = 1.6666666666666667; store_q2_q3_increase = 1.3333333333333333 }]))
