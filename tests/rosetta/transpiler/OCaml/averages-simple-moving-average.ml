(* Generated by Mochi transpiler v0.10.42 on 2025-07-27 17:23 +0700 *)


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

let rec indexOf s ch =
  let __ret = ref 0 in
  (try
  let i = ref (0) in
  (try while (!i < String.length (s)) do
    try
  if (String.sub (s) !i ((!i + 1) - !i) = ch) then (
  __ret := (Obj.magic (!i) : int); raise Return
  );
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (-(1)) : int); raise Return
  with Return -> !__ret)

let rec fmt3 x =
  let __ret = ref "" in
  (try
  let x = (Obj.magic x : float) in
  let y = ref ((float_of_int ((Obj.magic ((x *. 1000.0) +. 0.5) : int)) /. 1000.0)) in
  let s = ref ((string_of_float (!y))) in
  let dot = ref (indexOf (!s) (".")) in
  if (!dot = (0 - 1)) then (
  s := (!s ^ ".000");
  ) else (
  let decs = ref (((String.length (!s) - !dot) - 1)) in
  if (!decs > 3) then (
  s := String.sub (!s) 0 ((!dot + 4) - 0);
  ) else (
  (try while (!decs < 3) do
    try
  s := (!s ^ "0");
  decs := (!decs + 1);
    with Continue -> ()
  done with Break -> ());
  );
  );
  __ret := (Obj.magic (!s) : string); raise Return
  with Return -> !__ret)

let rec pad s width =
  let __ret = ref "" in
  (try
  let width = (Obj.magic width : int) in
  let out = ref (s) in
  (try while (String.length (!out) < width) do
    try
  out := (" " ^ !out);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!out) : string); raise Return
  with Return -> !__ret)

let rec smaSeries xs period =
  let __ret = ref ([] : float list) in
  (try
  let period = (Obj.magic period : int) in
  let res = ref ([]) in
  let sum = ref (0.0) in
  let i = ref (0) in
  (try while (!i < List.length (xs)) do
    try
  sum := (!sum +. List.nth (xs) (!i));
  if (!i >= period) then (
  sum := (!sum -. List.nth (xs) ((!i - period)));
  );
  let denom = ref ((!i + 1)) in
  if (!denom > period) then (
  denom := period;
  );
  res := List.append !res [(!sum /. float_of_int (!denom))];
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!res) : float list); raise Return
  with Return -> !__ret)

let rec main () =
  let __ret = ref (Obj.magic 0) in
  (try
  let xs = ref ([1.0; 2.0; 3.0; 4.0; 5.0; 5.0; 4.0; 3.0; 2.0; 1.0]) in
  let sma3 = ref (smaSeries (!xs) (3)) in
  let sma5 = ref (smaSeries (!xs) (5)) in
  print_endline ("x       sma3   sma5");
  let i = ref (0) in
  (try while (!i < List.length (!xs)) do
    try
  let line = ((((pad (fmt3 (List.nth (!xs) (!i))) (5) ^ "  ") ^ pad (fmt3 (List.nth (!sma3) (!i))) (5)) ^ "  ") ^ pad (fmt3 (List.nth (!sma5) (!i))) (5)) in
  print_endline (__show line);
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
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
