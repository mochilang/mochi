(* Generated by Mochi compiler v0.10.25 on 2025-07-15T04:50:17Z *)
let sum lst = List.fold_left (+) 0 lst
let sum_float lst = List.fold_left (+.) 0.0 lst

type record1 = { mutable price : float; mutable ret : float }

let store_sales : record1 list = [{ price = 20.; ret = 5. };{ price = 10.; ret = 2. };{ price = 5.; ret = 0. }]
let catalog_sales : record1 list = [{ price = 15.; ret = 3. };{ price = 8.; ret = 1. }]
let web_sales : record1 list = [{ price = 25.; ret = 5. };{ price = 15.; ret = 8. };{ price = 8.; ret = 2. }]
let total_profit : float = (((sum_float (let __res0 = ref [] in
  List.iter (fun (s : record1) ->
      __res0 := (s.price -. s.ret) :: !__res0;
  ) store_sales;
List.rev !__res0)
) +. (sum_float (let __res1 = ref [] in
  List.iter (fun (c : record1) ->
      __res1 := (c.price -. c.ret) :: !__res1;
  ) catalog_sales;
List.rev !__res1)
)) +. (sum_float (let __res2 = ref [] in
  List.iter (fun (w : record1) ->
      __res2 := (w.price -. w.ret) :: !__res2;
  ) web_sales;
List.rev !__res2)
))

let () =
  json total_profit;
  assert ((total_profit = 80.))
