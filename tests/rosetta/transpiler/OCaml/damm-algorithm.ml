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

let rec damm s =
  let __ret = ref false in
  (try
  let tbl = ref ([[0; 3; 1; 7; 5; 9; 8; 6; 4; 2]; [7; 0; 9; 2; 1; 5; 4; 8; 6; 3]; [4; 2; 0; 6; 8; 7; 1; 3; 5; 9]; [1; 7; 5; 0; 9; 8; 3; 4; 2; 6]; [6; 1; 2; 3; 0; 4; 5; 9; 7; 8]; [3; 6; 7; 4; 2; 0; 9; 5; 8; 1]; [5; 8; 6; 9; 7; 2; 0; 1; 3; 4]; [8; 9; 4; 5; 3; 6; 2; 0; 1; 7]; [9; 4; 3; 8; 6; 1; 7; 2; 0; 5]; [2; 5; 8; 1; 4; 3; 6; 7; 9; 0]]) in
  let digits = ref ([("0", 0); ("1", 1); ("2", 2); ("3", 3); ("4", 4); ("5", 5); ("6", 6); ("7", 7); ("8", 8); ("9", 9)]) in
  let interim = ref (0) in
  let i = ref (0) in
  (try while (!i < String.length (s)) do
    try
  let digit = (try List.assoc (String.sub (s) !i ((!i + 1) - !i)) !digits with Not_found -> (Obj.magic 0)) in
  let row = ref (List.nth (!tbl) (!interim)) in
  interim := List.nth (!row) (digit);
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic ((!interim = 0)) : bool); raise Return
  with Return -> !__ret)

and padLeft s width =
  let __ret = ref "" in
  (try
  let width = (Obj.magic width : int) in
  (try while (String.length (!s) < width) do
    try
  s := (" " ^ !s);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!s) : string); raise Return
  with Return -> !__ret)

and main () =
  let __ret = ref (Obj.magic 0) in
  (try
  (try List.iter (fun s ->
    try
  print_endline (((padLeft (ref (s)) (Obj.repr (6)) ^ "  ") ^ __show (damm (s))));
    with Continue -> ()) (["5724"; "5727"; "112946"; "112949"]) with Break -> ());
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