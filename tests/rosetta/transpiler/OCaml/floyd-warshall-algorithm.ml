(* Generated by Mochi transpiler v0.10.52 on 2025-08-01 15:22 +0700 *)


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

let rec main () =
  let __ret = ref (Obj.magic 0) in
  (try
  let _inf = 1000000000 in
  let n = 4 in
  let dist = ref (([] : int list list)) in
  let next = ref (([] : int list list)) in
  let i = ref (0) in
  (try while (!i < n) do
    try
  let row = ref (([] : int list)) in
  let nrow = ref (([] : int list)) in
  let j = ref (0) in
  (try while (!j < n) do
    try
  if (!i = !j) then (
  row := (List.append (!row) [(Obj.magic (0) : int)]);
  ) else (
  row := (List.append (!row) [(Obj.magic (_inf) : int)]);
  );
  nrow := (List.append (!nrow) [(Obj.magic ((0 - 1)) : int)]);
  j := (!j + 1);
    with Continue -> ()
  done with Break -> ());
  dist := (List.append (!dist) [(Obj.magic (!row) : int list)]);
  next := (List.append (!next) [(Obj.magic (!nrow) : int list)]);
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  dist := (List.mapi (fun __i __x -> if __i = 0 then (List.mapi (fun __i __x -> if __i = 2 then -(2) else __x) (List.nth (!dist) (0))) else __x) (!dist));
  next := (List.mapi (fun __i __x -> if __i = 0 then (List.mapi (fun __i __x -> if __i = 2 then 2 else __x) (List.nth (!next) (0))) else __x) (!next));
  dist := (List.mapi (fun __i __x -> if __i = 2 then (List.mapi (fun __i __x -> if __i = 3 then 2 else __x) (List.nth (!dist) (2))) else __x) (!dist));
  next := (List.mapi (fun __i __x -> if __i = 2 then (List.mapi (fun __i __x -> if __i = 3 then 3 else __x) (List.nth (!next) (2))) else __x) (!next));
  dist := (List.mapi (fun __i __x -> if __i = 3 then (List.mapi (fun __i __x -> if __i = 1 then -(1) else __x) (List.nth (!dist) (3))) else __x) (!dist));
  next := (List.mapi (fun __i __x -> if __i = 3 then (List.mapi (fun __i __x -> if __i = 1 then 1 else __x) (List.nth (!next) (3))) else __x) (!next));
  dist := (List.mapi (fun __i __x -> if __i = 1 then (List.mapi (fun __i __x -> if __i = 0 then 4 else __x) (List.nth (!dist) (1))) else __x) (!dist));
  next := (List.mapi (fun __i __x -> if __i = 1 then (List.mapi (fun __i __x -> if __i = 0 then 0 else __x) (List.nth (!next) (1))) else __x) (!next));
  dist := (List.mapi (fun __i __x -> if __i = 1 then (List.mapi (fun __i __x -> if __i = 2 then 3 else __x) (List.nth (!dist) (1))) else __x) (!dist));
  next := (List.mapi (fun __i __x -> if __i = 1 then (List.mapi (fun __i __x -> if __i = 2 then 2 else __x) (List.nth (!next) (1))) else __x) (!next));
  let k = ref (0) in
  (try while (!k < n) do
    try
  let i = ref (0) in
  (try while (!i < n) do
    try
  let j = ref (0) in
  (try while (!j < n) do
    try
  if ((List.nth (List.nth (!dist) (!i)) (!k) < _inf) && (List.nth (List.nth (!dist) (!k)) (!j) < _inf)) then (
  let alt = (List.nth (List.nth (!dist) (!i)) (!k) + List.nth (List.nth (!dist) (!k)) (!j)) in
  if (alt < List.nth (List.nth (!dist) (!i)) (!j)) then (
  dist := (List.mapi (fun __i __x -> if __i = !i then (List.mapi (fun __i __x -> if __i = !j then alt else __x) (List.nth (!dist) (!i))) else __x) (!dist));
  next := (List.mapi (fun __i __x -> if __i = !i then (List.mapi (fun __i __x -> if __i = !j then List.nth (List.nth (!next) (!i)) (!k) else __x) (List.nth (!next) (!i))) else __x) (!next));
  );
  );
  j := (!j + 1);
    with Continue -> ()
  done with Break -> ());
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  k := (!k + 1);
    with Continue -> ()
  done with Break -> ());
let rec path u v =
  let __ret = ref ([] : int list) in
  (try
  let u = (Obj.magic u : int) in
  let v = (Obj.magic v : int) in
  let ui = ref ((u - 1)) in
  let vi = ref ((v - 1)) in
  if (List.nth (List.nth (!next) (!ui)) (!vi) = (0 - 1)) then (
  __ret := (Obj.magic ([]) : int list); raise Return
  );
  let p = ref ([u]) in
  let cur = ref (!ui) in
  (try while (!cur <> !vi) do
    try
  cur := List.nth (List.nth (!next) (!cur)) (!vi);
  p := (List.append (!p) [(Obj.magic ((!cur + 1)) : int)]);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!p) : int list); raise Return
  with Return -> !__ret) in
let rec pathStr p =
  let __ret = ref "" in
  (try
  let s = ref ("") in
  let first = ref (true) in
  let idx = ref (0) in
  (try while (!idx < List.length (p)) do
    try
  let x = List.nth (p) (!idx) in
  if not (!first) then (
  s := (!s ^ " -> ");
  );
  s := (!s ^ (string_of_int (x)));
  first := false;
  idx := (!idx + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!s) : string); raise Return
  with Return -> !__ret) in
  print_endline ("pair\tdist\tpath");
  let a = ref (0) in
  (try while (!a < n) do
    try
  let b = ref (0) in
  (try while (!b < n) do
    try
  if (!a <> !b) then (
  print_endline ((((((((string_of_int ((!a + 1))) ^ " -> ") ^ (string_of_int ((!b + 1)))) ^ "\t") ^ (string_of_int (List.nth (List.nth (!dist) (!a)) (!b)))) ^ "\t") ^ pathStr (path ((!a + 1)) ((!b + 1)))));
  );
  b := (!b + 1);
    with Continue -> ()
  done with Break -> ());
  a := (!a + 1);
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