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

let rec nextPrime primes start =
  let __ret = ref 0 in
  (try
  let start = (Obj.magic start : int) in
  let n = ref (start) in
  (try while true do
    try
  let isP = ref (true) in
  let i = ref (0) in
  (try while (!i < List.length (primes)) do
    try
  let p = List.nth (primes) (!i) in
  if ((p * p) > !n) then (
  raise Break;
  );
  if ((!n mod p) = 0) then (
  isP := false;
  raise Break;
  );
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  if !isP then (
  __ret := (Obj.magic (!n) : int); raise Return
  );
  n := (!n + 2);
    with Continue -> ()
  done with Break -> ());
    !__ret
  with Return -> !__ret)

and main () =
  let __ret = ref (Obj.magic 0) in
  (try
  let primes = ref ([2]) in
  let cand = ref (3) in
  (try while (List.length (!primes) < 10000) do
    try
  cand := nextPrime (!primes) (Obj.repr (!cand));
  primes := (List.append (!primes) [(Obj.magic (!cand) : int)]);
  cand := (!cand + 2);
    with Continue -> ()
  done with Break -> ());
  let line = ref ("First twenty:") in
  let i = ref (0) in
  (try while (!i < 20) do
    try
  line := ((!line ^ " ") ^ (string_of_int (List.nth (!primes) (!i))));
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  print_endline (__show !line);
  let idx = ref (0) in
  (try while (List.nth (!primes) (!idx) <= 100) do
    try
  idx := (!idx + 1);
    with Continue -> ()
  done with Break -> ());
  line := ("Between 100 and 150: " ^ (string_of_int (List.nth (!primes) (!idx))));
  idx := (!idx + 1);
  (try while (List.nth (!primes) (!idx) < 150) do
    try
  line := ((!line ^ " ") ^ (string_of_int (List.nth (!primes) (!idx))));
  idx := (!idx + 1);
    with Continue -> ()
  done with Break -> ());
  print_endline (__show !line);
  (try while (List.nth (!primes) (!idx) <= 7700) do
    try
  idx := (!idx + 1);
    with Continue -> ()
  done with Break -> ());
  let count = ref (0) in
  (try while (List.nth (!primes) (!idx) < 8000) do
    try
  count := (!count + 1);
  idx := (!idx + 1);
    with Continue -> ()
  done with Break -> ());
  print_endline (("Number beween 7,700 and 8,000: " ^ (string_of_int (!count))));
  print_endline (("10,000th prime: " ^ (string_of_int (List.nth (!primes) (9999)))));
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