(* Generated by Mochi transpiler v0.10.41 on 2025-07-26 19:01 +0700 *)


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
  if is_int r then string_of_int (magic v) else
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

let rec skipWS p =
  let __ret = ref [] in
  (try
  let i = ref ((Obj.obj (List.assoc ("pos") !p) : int)) in
  (try while ((!i < String.length ((Obj.obj (List.assoc ("expr") !p) : string))) && (String.sub ((Obj.obj (List.assoc ("expr") !p) : string)) !i ((!i + 1) - !i) = " ")) do
    try
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  p := (("pos", Obj.repr !i) :: List.remove_assoc "pos" !p);
  __ret := (Obj.magic (!p) : (string * Obj.t) list); raise Return
  with Return -> !__ret)

let rec parseIntStr str =
  let __ret = ref 0 in
  (try
  let i = ref (0) in
  let n = ref (0) in
  (try while (!i < String.length (str)) do
    try
  n := (((!n * 10) + String.sub (str) !i ((!i + 1) - !i)) - 48);
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!n) : int); raise Return
  with Return -> !__ret)

let rec parseNumber p =
  let __ret = ref [] in
  (try
  p := skipWS (p);
  let start = ref ((Obj.obj (List.assoc ("pos") !p) : int)) in
  (try while ((Obj.obj (List.assoc ("pos") !p) : int) < String.length ((Obj.obj (List.assoc ("expr") !p) : string))) do
    try
  let ch = String.sub ((Obj.obj (List.assoc ("expr") !p) : string)) (Obj.obj (List.assoc ("pos") !p) : int) (((Obj.obj (List.assoc ("pos") !p) : int) + 1) - (Obj.obj (List.assoc ("pos") !p) : int)) in
  if ((ch >= "0") && (ch <= "9")) then (
  p := (("pos", Obj.repr ((Obj.obj (List.assoc ("pos") !p) : int) + 1)) :: List.remove_assoc "pos" !p);
  ) else (
  raise Break;
  );
    with Continue -> ()
  done with Break -> ());
  let token = String.sub ((Obj.obj (List.assoc ("expr") !p) : string)) !start ((Obj.obj (List.assoc ("pos") !p) : int) - !start) in
  __ret := (Obj.magic ([("v", Obj.repr parseIntStr (token)); ("p", Obj.repr !p)]) : (string * Obj.t) list); raise Return
  with Return -> !__ret)

let rec parseFactor p =
  let __ret = ref [] in
  (try
  p := skipWS (p);
  if (((Obj.obj (List.assoc ("pos") !p) : int) < String.length ((Obj.obj (List.assoc ("expr") !p) : string))) && (String.sub ((Obj.obj (List.assoc ("expr") !p) : string)) (Obj.obj (List.assoc ("pos") !p) : int) (((Obj.obj (List.assoc ("pos") !p) : int) + 1) - (Obj.obj (List.assoc ("pos") !p) : int)) = "(")) then (
  p := (("pos", Obj.repr ((Obj.obj (List.assoc ("pos") !p) : int) + 1)) :: List.remove_assoc "pos" !p);
  let r = ref (parseExpr (p)) in
  let v = ref ((Obj.obj (List.assoc ("v") !r) : int)) in
  p := (Obj.obj (List.assoc ("p") !r) : (string * Obj.t) list);
  p := skipWS (p);
  if (((Obj.obj (List.assoc ("pos") !p) : int) < String.length ((Obj.obj (List.assoc ("expr") !p) : string))) && (String.sub ((Obj.obj (List.assoc ("expr") !p) : string)) (Obj.obj (List.assoc ("pos") !p) : int) (((Obj.obj (List.assoc ("pos") !p) : int) + 1) - (Obj.obj (List.assoc ("pos") !p) : int)) = ")")) then (
  p := (("pos", Obj.repr ((Obj.obj (List.assoc ("pos") !p) : int) + 1)) :: List.remove_assoc "pos" !p);
  );
  __ret := (Obj.magic ([("v", Obj.repr !v); ("p", Obj.repr !p)]) : (string * Obj.t) list); raise Return
  );
  if (((Obj.obj (List.assoc ("pos") !p) : int) < String.length ((Obj.obj (List.assoc ("expr") !p) : string))) && (String.sub ((Obj.obj (List.assoc ("expr") !p) : string)) (Obj.obj (List.assoc ("pos") !p) : int) (((Obj.obj (List.assoc ("pos") !p) : int) + 1) - (Obj.obj (List.assoc ("pos") !p) : int)) = "-")) then (
  p := (("pos", Obj.repr ((Obj.obj (List.assoc ("pos") !p) : int) + 1)) :: List.remove_assoc "pos" !p);
  let r = ref (parseFactor (p)) in
  let v = ref ((Obj.obj (List.assoc ("v") !r) : int)) in
  p := (Obj.obj (List.assoc ("p") !r) : (string * Obj.t) list);
  __ret := (Obj.magic ([("v", Obj.repr -(!v)); ("p", Obj.repr !p)]) : (string * Obj.t) list); raise Return
  );
  __ret := (Obj.magic (parseNumber (p)) : (string * Obj.t) list); raise Return
  with Return -> !__ret)

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

let rec parsePower p =
  let __ret = ref [] in
  (try
  let r = ref (parseFactor (p)) in
  let v = ref ((Obj.obj (List.assoc ("v") !r) : int)) in
  p := (Obj.obj (List.assoc ("p") !r) : (string * Obj.t) list);
  (try while true do
    try
  p := skipWS (p);
  if (((Obj.obj (List.assoc ("pos") !p) : int) < String.length ((Obj.obj (List.assoc ("expr") !p) : string))) && (String.sub ((Obj.obj (List.assoc ("expr") !p) : string)) (Obj.obj (List.assoc ("pos") !p) : int) (((Obj.obj (List.assoc ("pos") !p) : int) + 1) - (Obj.obj (List.assoc ("pos") !p) : int)) = "^")) then (
  p := (("pos", Obj.repr ((Obj.obj (List.assoc ("pos") !p) : int) + 1)) :: List.remove_assoc "pos" !p);
  let r2 = ref (parseFactor (p)) in
  let rhs = ref ((Obj.obj (List.assoc ("v") !r2) : int)) in
  p := (Obj.obj (List.assoc ("p") !r2) : (string * Obj.t) list);
  v := powInt (!v) (!rhs);
  ) else (
  raise Break;
  );
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic ([("v", Obj.repr !v); ("p", Obj.repr !p)]) : (string * Obj.t) list); raise Return
  with Return -> !__ret)

let rec parseTerm p =
  let __ret = ref [] in
  (try
  let r = ref (parsePower (p)) in
  let v = ref ((Obj.obj (List.assoc ("v") !r) : int)) in
  p := (Obj.obj (List.assoc ("p") !r) : (string * Obj.t) list);
  (try while true do
    try
  p := skipWS (p);
  if ((Obj.obj (List.assoc ("pos") !p) : int) < String.length ((Obj.obj (List.assoc ("expr") !p) : string))) then (
  let op = String.sub ((Obj.obj (List.assoc ("expr") !p) : string)) (Obj.obj (List.assoc ("pos") !p) : int) (((Obj.obj (List.assoc ("pos") !p) : int) + 1) - (Obj.obj (List.assoc ("pos") !p) : int)) in
  if (op = "*") then (
  p := (("pos", Obj.repr ((Obj.obj (List.assoc ("pos") !p) : int) + 1)) :: List.remove_assoc "pos" !p);
  let r2 = ref (parsePower (p)) in
  let rhs = ref ((Obj.obj (List.assoc ("v") !r2) : int)) in
  p := (Obj.obj (List.assoc ("p") !r2) : (string * Obj.t) list);
  v := (!v * !rhs);
  raise Continue;
  );
  if (op = "/") then (
  p := (("pos", Obj.repr ((Obj.obj (List.assoc ("pos") !p) : int) + 1)) :: List.remove_assoc "pos" !p);
  let r2 = ref (parsePower (p)) in
  let rhs = ref ((Obj.obj (List.assoc ("v") !r2) : int)) in
  p := (Obj.obj (List.assoc ("p") !r2) : (string * Obj.t) list);
  v := (!v / !rhs);
  raise Continue;
  );
  );
  raise Break;
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic ([("v", Obj.repr !v); ("p", Obj.repr !p)]) : (string * Obj.t) list); raise Return
  with Return -> !__ret)

let rec parseExpr p =
  let __ret = ref [] in
  (try
  let r = ref (parseTerm (p)) in
  let v = ref ((Obj.obj (List.assoc ("v") !r) : int)) in
  p := (Obj.obj (List.assoc ("p") !r) : (string * Obj.t) list);
  (try while true do
    try
  p := skipWS (p);
  if ((Obj.obj (List.assoc ("pos") !p) : int) < String.length ((Obj.obj (List.assoc ("expr") !p) : string))) then (
  let op = String.sub ((Obj.obj (List.assoc ("expr") !p) : string)) (Obj.obj (List.assoc ("pos") !p) : int) (((Obj.obj (List.assoc ("pos") !p) : int) + 1) - (Obj.obj (List.assoc ("pos") !p) : int)) in
  if (op = "+") then (
  p := (("pos", Obj.repr ((Obj.obj (List.assoc ("pos") !p) : int) + 1)) :: List.remove_assoc "pos" !p);
  let r2 = ref (parseTerm (p)) in
  let rhs = ref ((Obj.obj (List.assoc ("v") !r2) : int)) in
  p := (Obj.obj (List.assoc ("p") !r2) : (string * Obj.t) list);
  v := (!v + !rhs);
  raise Continue;
  );
  if (op = "-") then (
  p := (("pos", Obj.repr ((Obj.obj (List.assoc ("pos") !p) : int) + 1)) :: List.remove_assoc "pos" !p);
  let r2 = ref (parseTerm (p)) in
  let rhs = ref ((Obj.obj (List.assoc ("v") !r2) : int)) in
  p := (Obj.obj (List.assoc ("p") !r2) : (string * Obj.t) list);
  v := (!v - !rhs);
  raise Continue;
  );
  );
  raise Break;
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic ([("v", Obj.repr !v); ("p", Obj.repr !p)]) : (string * Obj.t) list); raise Return
  with Return -> !__ret)

let rec evalExpr expr =
  let __ret = ref 0 in
  (try
  let p = ref ([("expr", Obj.repr expr); ("pos", Obj.repr 0)]) in
  let r = parseExpr (p) in
  __ret := (Obj.magic ((Obj.obj (List.assoc ("v") r) : int)) : int); raise Return
  with Return -> !__ret)

let rec main () =
  let __ret = ref (Obj.magic 0) in
  (try
  let expr = "2*(3-1)+2*5" in
  print_endline (((expr ^ " = ") ^ (string_of_int (evalExpr (expr)))));
    !__ret
  with Return -> !__ret)

let () =
  let mem_start = _mem () in
  let start = _now () in
  ignore (main ());
  let finish = _now () in
  let mem_end = _mem () in
  let dur = (finish - start) / 1000 in
  let mem_bytes = max 0 (mem_end - mem_start) in
  Printf.printf "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"%s\"\n}\n" dur mem_bytes "main";
