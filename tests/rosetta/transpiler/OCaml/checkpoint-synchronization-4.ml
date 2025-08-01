(* Generated by Mochi transpiler v0.10.52 on 2025-08-01 22:42 +0700 *)


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

let nMech = ref (5)
let detailsPerMech = ref (4)
let () =
  let mem_start = _mem () in
  let start = _now () in
  (try for mech = 1 to ((!nMech + 1) - 1) do
    try
  let id = mech in
  print_endline ((((("worker " ^ (string_of_int (id))) ^ " contracted to assemble ") ^ (string_of_int (!detailsPerMech))) ^ " details"));
  print_endline ((("worker " ^ (string_of_int (id))) ^ " enters shop"));
  let d = ref (0) in
  (try while (!d < !detailsPerMech) do
    try
  print_endline ((("worker " ^ (string_of_int (id))) ^ " assembling"));
  print_endline ((("worker " ^ (string_of_int (id))) ^ " completed detail"));
  d := (!d + 1);
    with Continue -> ()
  done with Break -> ());
  print_endline ((("worker " ^ (string_of_int (id))) ^ " leaves shop"));
  print_endline ((("mechanism " ^ (string_of_int (mech))) ^ " completed"));
    with Continue -> ()
  done with Break -> ());
  let finish = _now () in
  let mem_end = _mem () in
  let dur = (finish - start) / 1000 in
  let mem_bytes = max 0 (mem_end - mem_start) in
  Printf.printf "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"%s\"\n}\n" dur mem_bytes "main";
  ()