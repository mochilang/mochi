(* Generated by Mochi transpiler v0.10.52 on 2025-08-01 19:04 UTC *)


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
  if is_int r then
    let i = (magic v : int) in
    if i = 0 || i = 1 then string_of_bool (i <> 0)
    else string_of_int i
  else
  match tag r with
  | 0 -> if size r = 0 then "[]" else "[" ^ list_aux r ^ "]"
  | 252 -> (magic v : string)
  | 253 -> string_of_float (magic v)
  | _ -> "<value>"


let nil = Obj.repr 0


let _now_seed = ref 0
let _now_seeded = ref false

let _now () =
  if not !_now_seeded then (
    match Sys.getenv_opt "MOCHI_NOW_SEED" with
    | Some s -> (try _now_seed := int_of_string s; _now_seeded := true with _ -> ())
    | None -> ()
  );
  if !_now_seeded then (
    _now_seed := (!(_now_seed) * 1664525 + 1013904223) mod 2147483647;
    !_now_seed
  ) else int_of_float (Sys.time () *. 1000000000.)


let _mem () =
  int_of_float (Gc.allocated_bytes ())

exception Break
exception Continue

exception Return

let rec powInt base exp =
  let __ret = ref 0 in
  (try
  let base = (Obj.magic base : int) in
  let exp = (Obj.magic exp : int) in
  let r = ref (1) in
  let b = ref (base) in
  let e = ref (exp) in
  (try while (!e > 0) do
    try
  if ((!e mod 2) = 1) then (
  r := (!r * !b);
  );
  b := (!b * !b);
  e := (!e / 2);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!r) : int); raise Return
  with Return -> !__ret)

and minInt x y =
  let __ret = ref 0 in
  (try
  let x = (Obj.magic x : int) in
  let y = (Obj.magic y : int) in
  if (x < y) then (
  __ret := (Obj.magic (x) : int); raise Return
  );
  __ret := (Obj.magic (y) : int); raise Return
  with Return -> !__ret)

and throwDie nSides nDice s counts =
  let __ret = ref (Obj.magic 0) in
  (try
  let nSides = (Obj.magic nSides : int) in
  let nDice = (Obj.magic nDice : int) in
  let s = (Obj.magic s : int) in
  if (nDice = 0) then (
  counts := (List.mapi (fun __i __x -> if __i = s then (List.nth (!counts) (s) + 1) else __x) (!counts));
  __ret := (); raise Return
  );
  let i = ref (1) in
  (try while (!i <= nSides) do
    try
  ignore (throwDie (Obj.repr (nSides)) (Obj.repr ((nDice - 1))) (Obj.repr ((s + !i))) (counts));
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
    !__ret
  with Return -> !__ret)

and beatingProbability nSides1 nDice1 nSides2 nDice2 =
  let __ret = ref 0.0 in
  (try
  let nSides1 = (Obj.magic nSides1 : int) in
  let nDice1 = (Obj.magic nDice1 : int) in
  let nSides2 = (Obj.magic nSides2 : int) in
  let nDice2 = (Obj.magic nDice2 : int) in
  let len1 = ((nSides1 + 1) * nDice1) in
  let c1 = ref (([] : int list)) in
  let i = ref (0) in
  (try while (!i < len1) do
    try
  c1 := (List.append (!c1) [(Obj.magic (0) : int)]);
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  ignore (throwDie (Obj.repr (nSides1)) (Obj.repr (nDice1)) (Obj.repr (0)) (c1));
  let len2 = ((nSides2 + 1) * nDice2) in
  let c2 = ref (([] : int list)) in
  let j = ref (0) in
  (try while (!j < len2) do
    try
  c2 := (List.append (!c2) [(Obj.magic (0) : int)]);
  j := (!j + 1);
    with Continue -> ()
  done with Break -> ());
  ignore (throwDie (Obj.repr (nSides2)) (Obj.repr (nDice2)) (Obj.repr (0)) (c2));
  let p12 = (float_of_int (powInt (Obj.repr (nSides1)) (Obj.repr (nDice1))) *. float_of_int (powInt (Obj.repr (nSides2)) (Obj.repr (nDice2)))) in
  let tot = ref (0.0) in
  i := 0;
  (try while (!i < len1) do
    try
  j := 0;
  let m = minInt (Obj.repr (!i)) (Obj.repr (len2)) in
  (try while (!j < m) do
    try
  tot := (!tot +. ((float_of_int (List.nth (!c1) (!i)) *. float_of_int (List.nth (!c2) (!j))) /. p12));
  j := (!j + 1);
    with Continue -> ()
  done with Break -> ());
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!tot) : float); raise Return
  with Return -> !__ret)


let () =
  let mem_start = _mem () in
  let start = _now () in
  print_endline ((Printf.sprintf "%.16g" (beatingProbability (Obj.repr (4)) (Obj.repr (9)) (Obj.repr (6)) (Obj.repr (6)))));
  print_endline ((Printf.sprintf "%.16g" (beatingProbability (Obj.repr (10)) (Obj.repr (5)) (Obj.repr (7)) (Obj.repr (6)))));
  let finish = _now () in
  let mem_end = _mem () in
  let dur = (finish - start) / 1000 in
  let mem_bytes = max 0 (mem_end - mem_start) in
  Printf.printf "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"%s\"\n}\n" dur mem_bytes "main";
  ()