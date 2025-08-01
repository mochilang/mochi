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

let rec splitLines s =
  let __ret = ref ([] : string list) in
  (try
  let lines = ref (([] : string list)) in
  let start = ref (0) in
  let i = ref (0) in
  (try while (!i < String.length (s)) do
    try
  if (String.sub (s) !i ((!i + 1) - !i) = "\n") then (
  lines := (List.append (!lines) [(Obj.magic (String.sub (s) !start (!i - !start)) : string)]);
  i := (!i + 1);
  start := !i;
  ) else (
  i := (!i + 1);
  );
    with Continue -> ()
  done with Break -> ());
  lines := (List.append (!lines) [(Obj.magic (String.sub (s) !start (String.length (s) - !start)) : string)]);
  __ret := (Obj.magic (!lines) : string list); raise Return
  with Return -> !__ret)

and parseFasta text =
  let __ret = ref ([] : string list) in
  (try
  let key = ref ("") in
  let val_ = ref ("") in
  let out = ref (([] : string list)) in
  (try List.iter (fun line ->
    try
  if (line = "") then (
  raise Continue;
  );
  if (String.sub (line) 0 (1 - 0) = ">") then (
  if (!key <> "") then (
  out := (List.append (!out) [(Obj.magic (((!key ^ ": ") ^ !val_)) : string)]);
  );
  let hdr = ref (String.sub (line) 1 (String.length (line) - 1)) in
  let idx = ref (0) in
  (try while ((!idx < String.length (!hdr)) && (String.sub (!hdr) !idx ((!idx + 1) - !idx) <> " ")) do
    try
  idx := (!idx + 1);
    with Continue -> ()
  done with Break -> ());
  key := String.sub (!hdr) 0 (!idx - 0);
  val_ := "";
  ) else (
  if (!key = "") then (
  print_endline ("missing header");
  __ret := (Obj.magic ([]) : string list); raise Return
  );
  val_ := (!val_ ^ line);
  );
    with Continue -> ()) (splitLines (text)) with Break -> ());
  if (!key <> "") then (
  out := (List.append (!out) [(Obj.magic (((!key ^ ": ") ^ !val_)) : string)]);
  );
  __ret := (Obj.magic (!out) : string list); raise Return
  with Return -> !__ret)

and main () =
  let __ret = ref (Obj.magic 0) in
  (try
  let res = ref (parseFasta (_fasta)) in
  (try List.iter (fun line ->
    try
  print_endline (__show line);
    with Continue -> ()) (!res) with Break -> ());
    !__ret
  with Return -> !__ret)


let () =
  let mem_start = _mem () in
  let start = _now () in
  let _fasta = (((((">Rosetta_Example_1\n" ^ "THERECANBENOSPACE\n") ^ ">Rosetta_Example_2\n") ^ "THERECANBESEVERAL\n") ^ "LINESBUTTHEYALLMUST\n") ^ "BECONCATENATED") in
  ignore (main ());
  let finish = _now () in
  let mem_end = _mem () in
  let dur = (finish - start) / 1000 in
  let mem_bytes = max 0 (mem_end - mem_start) in
  Printf.printf "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"%s\"\n}\n" dur mem_bytes "main";
  ()