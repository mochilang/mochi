(* Generated by Mochi transpiler v0.10.52 on 2025-08-02 00:57 +0700 *)


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

let rec xor a b =
  let __ret = ref 0 in
  (try
  let a = (Obj.magic a : int) in
  let b = (Obj.magic b : int) in
  let res = ref (0) in
  let bit = ref (1) in
  let x = ref (a) in
  let y = ref (b) in
  (try while ((!x > 0) || (!y > 0)) do
    try
  let abit = (!x mod 2) in
  let bbit = (!y mod 2) in
  if (abit <> bbit) then (
  res := (!res + !bit);
  );
  x := (!x / 2);
  y := (!y / 2);
  bit := (!bit * 2);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!res) : int); raise Return
  with Return -> !__ret)

and rshift x n =
  let __ret = ref 0 in
  (try
  let x = (Obj.magic x : int) in
  let n = (Obj.magic n : int) in
  let v = ref (x) in
  let i = ref (0) in
  (try while (!i < n) do
    try
  v := (!v / 2);
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!v) : int); raise Return
  with Return -> !__ret)

and ord ch =
  let __ret = ref 0 in
  (try
  let upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
  let lower = "abcdefghijklmnopqrstuvwxyz" in
  let idx = ref ((try String.index (upper) (String.get (ch) 0) with Not_found -> -1)) in
  if (!idx >= 0) then (
  __ret := (Obj.magic ((65 + !idx)) : int); raise Return
  );
  idx := (try String.index (lower) (String.get (ch) 0) with Not_found -> -1);
  if (!idx >= 0) then (
  __ret := (Obj.magic ((97 + !idx)) : int); raise Return
  );
  if (ch = " ") then (
  __ret := (Obj.magic (32) : int); raise Return
  );
  __ret := (Obj.magic (0) : int); raise Return
  with Return -> !__ret)

and toHex n =
  let __ret = ref "" in
  (try
  let n = (Obj.magic n : int) in
  let digits = "0123456789ABCDEF" in
  if (n = 0) then (
  __ret := (Obj.magic ("0") : string); raise Return
  );
  let v = ref (n) in
  let out = ref ("") in
  (try while (!v > 0) do
    try
  let d = (!v mod 16) in
  out := (String.sub (digits) d ((d + 1) - d) ^ !out);
  v := (!v / 16);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!out) : string); raise Return
  with Return -> !__ret)

and crc32Table () =
  let __ret = ref ([] : int list) in
  (try
  let table = ref (([] : int list)) in
  let i = ref (0) in
  (try while (!i < 256) do
    try
  let word = ref (!i) in
  let j = ref (0) in
  (try while (!j < 8) do
    try
  if ((!word mod 2) = 1) then (
  word := xor (Obj.repr (rshift (Obj.repr (!word)) (Obj.repr (1)))) (Obj.repr (3988292384));
  ) else (
  word := rshift (Obj.repr (!word)) (Obj.repr (1));
  );
  j := (!j + 1);
    with Continue -> ()
  done with Break -> ());
  table := (List.append (!table) [(Obj.magic (!word) : int)]);
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!table) : int list); raise Return
  with Return -> !__ret)


let table = ref (crc32Table ())
let rec crc32 s =
  let __ret = ref 0 in
  (try
  let crc = ref (4294967295) in
  let i = ref (0) in
  (try while (!i < String.length (s)) do
    try
  let c = ord (String.sub (s) !i ((!i + 1) - !i)) in
  let idx = xor (Obj.repr ((!crc mod 256))) (Obj.repr (c)) in
  crc := xor (Obj.repr (List.nth (!table) (idx))) (Obj.repr (rshift (Obj.repr (!crc)) (Obj.repr (8))));
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic ((4294967295 - !crc)) : int); raise Return
  with Return -> !__ret)

and main () =
  let __ret = ref (Obj.magic 0) in
  (try
  let s = "The quick brown fox jumps over the lazy dog" in
  let result = crc32 (s) in
  let hex = String.lowercase_ascii (toHex (Obj.repr (result))) in
  print_endline (__show hex);
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