(* Generated by Mochi transpiler v0.10.52 on 2025-08-01 18:57 +0700 *)


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

let rec makeInf () =
  let __ret = ref 0.0 in
  (try
  let x = ref (1.0) in
  let i = ref (0) in
  (try while (!i < 400) do
    try
  x := (!x *. 10.0);
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!x) : float); raise Return
  with Return -> !__ret)

and makeMax () =
  let __ret = ref 0.0 in
  (try
  let x = ref (1.0) in
  let i = ref (0) in
  (try while (!i < 308) do
    try
  x := (!x *. 10.0);
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!x) : float); raise Return
  with Return -> !__ret)

and isNaN x =
  let __ret = ref false in
  (try
  let x = (Obj.magic x : float) in
  __ret := (Obj.magic ((x <> x)) : bool); raise Return
  with Return -> !__ret)

and validateNaN n op =
  let __ret = ref (Obj.magic 0) in
  (try
  let n = (Obj.magic n : float) in
  if isNaN (Obj.repr (n)) then (
  print_endline ((op ^ " -> NaN"));
  ) else (
  print_endline ("!!! Expected NaN from");
  print_endline (__show op);
  print_endline (" Found");
  print_endline (Printf.sprintf "%.15f" (n));
  );
    !__ret
  with Return -> !__ret)

and validateZero n op =
  let __ret = ref (Obj.magic 0) in
  (try
  let n = (Obj.magic n : float) in
  if (n = float_of_int (0)) then (
  print_endline ((op ^ " -> 0"));
  ) else (
  print_endline ("!!! Expected 0 from");
  print_endline (__show op);
  print_endline (" Found");
  print_endline (Printf.sprintf "%.15f" (n));
  );
    !__ret
  with Return -> !__ret)

and validateGT a b op =
  let __ret = ref (Obj.magic 0) in
  (try
  let a = (Obj.magic a : float) in
  let b = (Obj.magic b : float) in
  if (a > b) then (
  print_endline (__show op);
  ) else (
  print_endline ("!!! Expected");
  print_endline (__show op);
  print_endline (" Found not true.");
  );
    !__ret
  with Return -> !__ret)

and validateNE a b op =
  let __ret = ref (Obj.magic 0) in
  (try
  let a = (Obj.magic a : float) in
  let b = (Obj.magic b : float) in
  if (a = b) then (
  print_endline ("!!! Expected");
  print_endline (__show op);
  print_endline (" Found not true.");
  ) else (
  print_endline (__show op);
  );
    !__ret
  with Return -> !__ret)

and validateEQ a b op =
  let __ret = ref (Obj.magic 0) in
  (try
  let a = (Obj.magic a : float) in
  let b = (Obj.magic b : float) in
  if (a = b) then (
  print_endline (__show op);
  ) else (
  print_endline ("!!! Expected");
  print_endline (__show op);
  print_endline (" Found not true.");
  );
    !__ret
  with Return -> !__ret)

and main () =
  let __ret = ref (Obj.magic 0) in
  (try
  let negZero = (-.(0.0)) in
  let posInf = makeInf () in
  let negInf = (-.(posInf)) in
  let nan = (posInf /. posInf) in
  let maxVal = makeMax () in
  print_endline (Printf.sprintf "%.15f" (negZero));
  print_endline (Printf.sprintf "%.15f" (posInf));
  print_endline (Printf.sprintf "%.15f" (negInf));
  print_endline (Printf.sprintf "%.15f" (nan));
  print_endline (Printf.sprintf "%.15f" (negZero));
  print_endline (Printf.sprintf "%.15f" (posInf));
  print_endline (Printf.sprintf "%.15f" (negInf));
  print_endline (Printf.sprintf "%.15f" (nan));
  print_endline ("");
  ignore (validateNaN (Obj.repr ((negInf +. posInf))) ("-Inf + Inf"));
  ignore (validateNaN (Obj.repr ((0.0 *. posInf))) ("0 * Inf"));
  ignore (validateNaN (Obj.repr ((posInf /. posInf))) ("Inf / Inf"));
  ignore (validateNaN (Obj.repr ((posInf mod 1.0))) ("Inf % 1"));
  ignore (validateNaN (Obj.repr ((1.0 +. nan))) ("1 + NaN"));
  ignore (validateZero (Obj.repr ((1.0 /. posInf))) ("1 / Inf"));
  ignore (validateGT (Obj.repr (posInf)) (Obj.repr (maxVal)) ("Inf > max value"));
  ignore (validateGT (Obj.repr ((-.(maxVal)))) (Obj.repr (negInf)) ("-Inf < max neg value"));
  ignore (validateNE (Obj.repr (nan)) (Obj.repr (nan)) ("NaN != NaN"));
  ignore (validateEQ (Obj.repr (negZero)) (Obj.repr (0.0)) ("-0 == 0"));
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
  ()