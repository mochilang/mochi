(* Generated by Mochi transpiler v0.10.55 on 2025-08-02 18:27 +0700 *)


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

let vals = ref ([0; 2; 4; 6; 30; 32; 34; 36; 40; 42; 44; 46; 50; 52; 54; 56; 60; 62; 64; 66])
let billions = ref ([0; 2; 4; 6])
let rec ebanNumbers start stop =
  let __ret = ref ([] : int list) in
  (try
  let start = (Obj.magic start : int) in
  let stop = (Obj.magic stop : int) in
  let nums = ref (([] : int list)) in
  (try List.iter (fun b ->
    try
  (try List.iter (fun m ->
    try
  (try List.iter (fun t ->
    try
  (try List.iter (fun r ->
    try
  let n = ((((b * 1000000000) + (m * 1000000)) + (t * 1000)) + r) in
  if ((n >= start) && (n <= stop)) then (
  nums := (List.append (!nums) [(Obj.magic (n) : int)]);
  );
    with Continue -> ()) (!vals) with Break -> ());
    with Continue -> ()) (!vals) with Break -> ());
    with Continue -> ()) (!vals) with Break -> ());
    with Continue -> ()) (!billions) with Break -> ());
  __ret := (Obj.magic (!nums) : int list); raise Return
  with Return -> !__ret)

and countEban start stop =
  let __ret = ref 0 in
  (try
  let start = (Obj.magic start : int) in
  let stop = (Obj.magic stop : int) in
  let count = ref (0) in
  (try List.iter (fun b ->
    try
  (try List.iter (fun m ->
    try
  (try List.iter (fun t ->
    try
  (try List.iter (fun r ->
    try
  let n = ((((b * 1000000000) + (m * 1000000)) + (t * 1000)) + r) in
  if ((n >= start) && (n <= stop)) then (
  count := (!count + 1);
  );
    with Continue -> ()) (!vals) with Break -> ());
    with Continue -> ()) (!vals) with Break -> ());
    with Continue -> ()) (!vals) with Break -> ());
    with Continue -> ()) (!billions) with Break -> ());
  __ret := (Obj.magic (!count) : int); raise Return
  with Return -> !__ret)

and main () =
  let __ret = ref (Obj.magic 0) in
  (try
  let ranges = ref ([[Obj.repr (2); Obj.repr (1000); Obj.repr (true)]; [Obj.repr (1000); Obj.repr (4000); Obj.repr (true)]; [Obj.repr (2); Obj.repr (10000); Obj.repr (false)]; [Obj.repr (2); Obj.repr (100000); Obj.repr (false)]; [Obj.repr (2); Obj.repr (1000000); Obj.repr (false)]; [Obj.repr (2); Obj.repr (10000000); Obj.repr (false)]; [Obj.repr (2); Obj.repr (100000000); Obj.repr (false)]; [Obj.repr (2); Obj.repr (1000000000); Obj.repr (false)]]) in
  (try List.iter (fun rg ->
    try
  let start = List.nth (rg) (0) in
  let stop = List.nth (rg) (1) in
  let show = (Obj.magic List.nth (rg) (2) : bool) in
  if (start = 2) then (
  print_endline ((("eban numbers up to and including " ^ (string_of_int (stop))) ^ ":"));
  ) else (
  print_endline ((((("eban numbers between " ^ (string_of_int (start))) ^ " and ") ^ (string_of_int (stop))) ^ " (inclusive):"));
  );
  if show then (
  let nums = ref (ebanNumbers (Obj.repr (start)) (Obj.repr (stop))) in
  let line = ref ("") in
  let i = ref (0) in
  (try while (!i < List.length (!nums)) do
    try
  line := ((!line ^ (string_of_int (List.nth (!nums) (!i)))) ^ " ");
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  if (String.length (!line) > 0) then (
  print_endline (String.sub (!line) 0 ((String.length (!line) - 1) - 0));
  );
  );
  let c = countEban (Obj.repr (start)) (Obj.repr (stop)) in
  print_endline ((("count = " ^ (string_of_int (c))) ^ "\n"));
    with Continue -> ()) (!ranges) with Break -> ());
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