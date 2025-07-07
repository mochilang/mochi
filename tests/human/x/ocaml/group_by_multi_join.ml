(* Hand-written OCaml version of group_by_multi_join.mochi *)

type nation = { id : int; name : string }
let nations = [
  { id = 1; name = "A" };
  { id = 2; name = "B" };
]

type supplier = { id : int; nation : int }
let suppliers = [
  { id = 1; nation = 1 };
  { id = 2; nation = 2 };
]

type partsupp = { part : int; supplier : int; cost : float; qty : int }
let partsupp = [
  { part = 100; supplier = 1; cost = 10.0; qty = 2 };
  { part = 100; supplier = 2; cost = 20.0; qty = 1 };
  { part = 200; supplier = 1; cost = 5.0; qty = 3 };
]

(* join nations and suppliers then filter *)
let filtered =
  List.fold_left (fun acc ps ->
    let s = List.find (fun s -> s.id = ps.supplier) suppliers in
    let n = List.find (fun n -> n.id = s.nation) nations in
    if n.name = "A" then
      let value = ps.cost *. float_of_int ps.qty in
      (ps.part, value) :: acc
    else acc
  ) [] partsupp
  |> List.rev

(* group by part and sum values *)
let totals =
  List.fold_left (fun map (part, value) ->
    let cur = try List.assoc part map with Not_found -> 0.0 in
    (part, cur +. value) :: List.remove_assoc part map
  ) [] filtered

let () =
  List.iter (fun (part,total) ->
    Printf.printf "{part=%d; total=%g}\n" part total
  ) totals
