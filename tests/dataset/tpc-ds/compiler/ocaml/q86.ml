(* Generated by Mochi compiler v0.10.25 on 2025-07-15T04:50:17Z *)
let sum lst = List.fold_left (+) 0 lst
let sum_float lst = List.fold_left (+.) 0.0 lst

type record1 = { mutable cat : string; mutable class : string; mutable net : float }

let web_sales : record1 list = [{ cat = "A"; class = "B"; net = 40. };{ cat = "A"; class = "B"; net = 46. };{ cat = "A"; class = "C"; net = 10. };{ cat = "B"; class = "B"; net = 20. }]
let result : float = (sum_float (let __res0 = ref [] in
  List.iter (fun (ws : record1) ->
      if ((ws.cat = "A") && (ws.class = "B")) then
    __res0 := ws.net :: !__res0;
  ) web_sales;
List.rev !__res0)
)

let () =
  json result;
  assert ((result = 86.))
