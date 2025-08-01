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

let _pi = 3.141592653589793
let rec conv2d img k =
  let __ret = ref ([] : float list list) in
  (try
  let h = List.length (img) in
  let w = List.length (List.nth (img) (0)) in
  let n = List.length (k) in
  let half = (n / 2) in
  let out = ref (([] : float list list)) in
  let y = ref (0) in
  (try while (!y < h) do
    try
  let row = ref (([] : float list)) in
  let x = ref (0) in
  (try while (!x < w) do
    try
  let sum = ref (0.0) in
  let j = ref (0) in
  (try while (!j < n) do
    try
  let i = ref (0) in
  (try while (!i < n) do
    try
  let yy = ref (((!y + !j) - half)) in
  if (!yy < 0) then (
  yy := 0;
  );
  if (!yy >= h) then (
  yy := (h - 1);
  );
  let xx = ref (((!x + !i) - half)) in
  if (!xx < 0) then (
  xx := 0;
  );
  if (!xx >= w) then (
  xx := (w - 1);
  );
  sum := (!sum +. (List.nth (List.nth (img) (!yy)) (!xx) *. List.nth (List.nth (k) (!j)) (!i)));
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  j := (!j + 1);
    with Continue -> ()
  done with Break -> ());
  row := (List.append (!row) [(Obj.magic (!sum) : float)]);
  x := (!x + 1);
    with Continue -> ()
  done with Break -> ());
  out := (List.append (!out) [(Obj.magic (!row) : float list)]);
  y := (!y + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!out) : float list list); raise Return
  with Return -> !__ret)

and gradient img =
  let __ret = ref ([] : float list list) in
  (try
  let hx = ref ([[(-.(1.0)); 0.0; 1.0]; [(-.(2.0)); 0.0; 2.0]; [(-.(1.0)); 0.0; 1.0]]) in
  let hy = ref ([[1.0; 2.0; 1.0]; [0.0; 0.0; 0.0]; [(-.(1.0)); (-.(2.0)); (-.(1.0))]]) in
  let gx = ref (conv2d (img) (!hx)) in
  let gy = ref (conv2d (img) (!hy)) in
  let h = ref (List.length (img)) in
  let w = ref (List.length (List.nth (img) (0))) in
  let out = ref (([] : float list list)) in
  let y = ref (0) in
  (try while (!y < !h) do
    try
  let row = ref (([] : float list)) in
  let x = ref (0) in
  (try while (!x < !w) do
    try
  let g = ((List.nth (List.nth (!gx) (!y)) (!x) *. List.nth (List.nth (!gx) (!y)) (!x)) +. (List.nth (List.nth (!gy) (!y)) (!x) *. List.nth (List.nth (!gy) (!y)) (!x))) in
  row := (List.append (!row) [(Obj.magic (g) : float)]);
  x := (!x + 1);
    with Continue -> ()
  done with Break -> ());
  out := (List.append (!out) [(Obj.magic (!row) : float list)]);
  y := (!y + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!out) : float list list); raise Return
  with Return -> !__ret)

and threshold g t =
  let __ret = ref ([] : int list list) in
  (try
  let t = (Obj.magic t : float) in
  let h = ref (List.length (g)) in
  let w = ref (List.length (List.nth (g) (0))) in
  let out = ref (([] : int list list)) in
  let y = ref (0) in
  (try while (!y < !h) do
    try
  let row = ref (([] : int list)) in
  let x = ref (0) in
  (try while (!x < !w) do
    try
  if (List.nth (List.nth (g) (!y)) (!x) >= t) then (
  row := (List.append (!row) [(Obj.magic (1) : int)]);
  ) else (
  row := (List.append (!row) [(Obj.magic (0) : int)]);
  );
  x := (!x + 1);
    with Continue -> ()
  done with Break -> ());
  out := (List.append (!out) [(Obj.magic (!row) : int list)]);
  y := (!y + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!out) : int list list); raise Return
  with Return -> !__ret)

and printMatrix m =
  let __ret = ref (Obj.magic 0) in
  (try
  let y = ref (0) in
  (try while (!y < List.length (m)) do
    try
  let line = ref ("") in
  let x = ref (0) in
  (try while (!x < List.length (List.nth (m) (0))) do
    try
  line := (!line ^ (string_of_int (List.nth (List.nth (m) (!y)) (!x))));
  if (!x < (List.length (List.nth (m) (0)) - 1)) then (
  line := (!line ^ " ");
  );
  x := (!x + 1);
    with Continue -> ()
  done with Break -> ());
  print_endline (__show !line);
  y := (!y + 1);
    with Continue -> ()
  done with Break -> ());
    !__ret
  with Return -> !__ret)

and main () =
  let __ret = ref (Obj.magic 0) in
  (try
  let img = ref ([[0.0; 0.0; 0.0; 0.0; 0.0]; [0.0; 255.0; 255.0; 255.0; 0.0]; [0.0; 255.0; 255.0; 255.0; 0.0]; [0.0; 255.0; 255.0; 255.0; 0.0]; [0.0; 0.0; 0.0; 0.0; 0.0]]) in
  let g = ref (gradient (!img)) in
  let edges = ref (threshold (!g) (Obj.repr ((1020.0 *. 1020.0)))) in
  ignore (printMatrix (!edges));
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