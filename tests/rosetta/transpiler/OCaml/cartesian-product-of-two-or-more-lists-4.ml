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

let rec listStr xs =
  let __ret = ref "" in
  (try
  let s = ref ("[") in
  let i = ref (0) in
  (try while (!i < List.length (xs)) do
    try
  s := (!s ^ (string_of_int (List.nth (xs) (!i))));
  if (!i < (List.length (xs) - 1)) then (
  s := (!s ^ " ");
  );
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  s := (!s ^ "]");
  __ret := (Obj.magic (!s) : string); raise Return
  with Return -> !__ret)

and llStr lst =
  let __ret = ref "" in
  (try
  let s = ref ("[") in
  let i = ref (0) in
  (try while (!i < List.length (lst)) do
    try
  s := (!s ^ listStr (List.nth (lst) (!i)));
  if (!i < (List.length (lst) - 1)) then (
  s := (!s ^ " ");
  );
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  s := (!s ^ "]");
  __ret := (Obj.magic (!s) : string); raise Return
  with Return -> !__ret)

and copy xs =
  let __ret = ref ([] : int list) in
  (try
  let out = ref (([] : int list)) in
  (try List.iter (fun v ->
    try
  out := (List.append (!out) [(Obj.magic (v) : int)]);
    with Continue -> ()) (xs) with Break -> ());
  __ret := (Obj.magic (!out) : int list); raise Return
  with Return -> !__ret)

and cartN lists =
  let __ret = ref ([] : int list list) in
  (try
  if (lists = nil) then (
  __ret := (Obj.magic ([]) : int list list); raise Return
  );
  let a = ref ((Obj.magic lists : int list list)) in
  if (List.length (!a) = 0) then (
  __ret := (Obj.magic ([[]]) : int list list); raise Return
  );
  let out = ref (([] : int list list)) in
  let last = (List.length (!a) - 1) in
  let left = ref (cartN ((Obj.repr (List.map (fun v -> Obj.repr v) (List.of_seq (Seq.take (last - 0) (Seq.drop 0 (List.to_seq !a)))))))) in
  (try List.iter (fun p ->
    try
  (try List.iter (fun x ->
    try
  let row = ref (copy (p)) in
  row := (List.append (!row) [(Obj.magic (x) : int)]);
  out := (List.append (!out) [(Obj.magic (!row) : int list)]);
    with Continue -> ()) (List.nth (!a) (last)) with Break -> ());
    with Continue -> ()) (!left) with Break -> ());
  __ret := (Obj.magic (!out) : int list list); raise Return
  with Return -> !__ret)

and main () =
  let __ret = ref (Obj.magic 0) in
  (try
  print_endline (llStr (cartN ((Obj.repr (List.map (fun v -> Obj.repr v) ([[1; 2]; [3; 4]]))))));
  print_endline (llStr (cartN ((Obj.repr (List.map (fun v -> Obj.repr v) ([[3; 4]; [1; 2]]))))));
  print_endline (llStr (cartN ((Obj.repr (List.map (fun v -> Obj.repr v) ([Obj.repr ([1; 2]); Obj.repr ([])]))))));
  print_endline (llStr (cartN ((Obj.repr (List.map (fun v -> Obj.repr v) ([Obj.repr ([]); Obj.repr ([1; 2])]))))));
  print_endline ("");
  print_endline ("[");
  (try List.iter (fun p ->
    try
  print_endline ((" " ^ listStr (p)));
    with Continue -> ()) (cartN ((Obj.repr (List.map (fun v -> Obj.repr v) ([[1776; 1789]; [7; 12]; [4; 14; 23]; [0; 1]]))))) with Break -> ());
  print_endline ("]");
  print_endline (llStr (cartN ((Obj.repr (List.map (fun v -> Obj.repr v) ([[1; 2; 3]; [30]; [500; 100]]))))));
  print_endline (llStr (cartN ((Obj.repr (List.map (fun v -> Obj.repr v) ([Obj.repr ([1; 2; 3]); Obj.repr ([]); Obj.repr ([500; 100])]))))));
  print_endline ("");
  print_endline (llStr (cartN (nil)));
  print_endline (llStr (cartN ((Obj.repr (List.map (fun v -> Obj.repr v) ([]))))));
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