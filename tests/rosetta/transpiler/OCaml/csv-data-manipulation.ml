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

let rec join xs sep =
  let __ret = ref "" in
  (try
  let res = ref ("") in
  let i = ref (0) in
  (try while (!i < List.length (xs)) do
    try
  if (!i > 0) then (
  res := (!res ^ sep);
  );
  res := (!res ^ List.nth (xs) (!i));
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!res) : string); raise Return
  with Return -> !__ret)

and parseIntStr str =
  let __ret = ref 0 in
  (try
  let i = ref (0) in
  let neg = ref (false) in
  if ((String.length (str) > 0) && (String.sub (str) 0 (1 - 0) = "-")) then (
  neg := true;
  i := 1;
  );
  let n = ref (0) in
  let digits = ref ([("0", 0); ("1", 1); ("2", 2); ("3", 3); ("4", 4); ("5", 5); ("6", 6); ("7", 7); ("8", 8); ("9", 9)]) in
  (try while (!i < String.length (str)) do
    try
  n := ((!n * 10) + (try List.assoc (String.sub (str) !i ((!i + 1) - !i)) !digits with Not_found -> (Obj.magic 0)));
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  if !neg then (
  n := -(!n);
  );
  __ret := (Obj.magic (!n) : int); raise Return
  with Return -> !__ret)


let rows = ref ([["A"; "B"; "C"]; ["1"; "2"; "3"]; ["4"; "5"; "6"]; ["7"; "8"; "9"]])
let i = ref (1)
let () =
  let mem_start = _mem () in
  let start = _now () in
  rows := (List.mapi (fun __i __x -> if __i = 0 then (List.append (List.nth (!rows) (0)) [(Obj.magic ("SUM") : string)]) else __x) (!rows));
  (try while (!i < List.length (!rows)) do
    try
  let sum = ref (0) in
  (try List.iter (fun s ->
    try
  sum := (!sum + parseIntStr (s));
    with Continue -> ()) (List.nth (!rows) (!i)) with Break -> ());
  rows := (List.mapi (fun __i __x -> if __i = !i then (List.append (List.nth (!rows) (!i)) [(Obj.magic ((string_of_int (!sum))) : string)]) else __x) (!rows));
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  (try List.iter (fun r ->
    try
  print_endline (join (r) (","));
    with Continue -> ()) (!rows) with Break -> ());
  let finish = _now () in
  let mem_end = _mem () in
  let dur = (finish - start) / 1000 in
  let mem_bytes = max 0 (mem_end - mem_start) in
  Printf.printf "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"%s\"\n}\n" dur mem_bytes "main";
  ()