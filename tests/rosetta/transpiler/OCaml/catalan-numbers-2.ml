(* Generated by Mochi transpiler v0.10.52 on 2025-08-01 21:30 +0700 *)


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

let rec catalanRec n =
  let __ret = ref 0 in
  (try
  let n = (Obj.magic n : int) in
  if (n = 0) then (
  __ret := (Obj.magic (1) : int); raise Return
  );
  let t1 = ref ((2 * n)) in
  let t2 = ref ((!t1 - 1)) in
  let t3 = ref ((2 * !t2)) in
  let t5 = ref ((!t3 * catalanRec (Obj.repr ((n - 1))))) in
  __ret := (Obj.magic ((!t5 / (n + 1))) : int); raise Return
  with Return -> !__ret)

and main () =
  let __ret = ref (Obj.magic 0) in
  (try
  (try for i = 1 to (16 - 1) do
    try
  print_endline ((string_of_int (catalanRec (Obj.repr (i)))));
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
  ()