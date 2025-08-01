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

let rec sqrtApprox x =
  let __ret = ref 0.0 in
  (try
  let x = (Obj.magic x : float) in
  let guess = ref (x) in
  let i = ref (0) in
  (try while (!i < 20) do
    try
  guess := ((!guess +. (x /. !guess)) /. 2.0);
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!guess) : float); raise Return
  with Return -> !__ret)

and makeSym order elements =
  let __ret = ref [] in
  (try
  let order = (Obj.magic order : int) in
  __ret := (Obj.magic ([("order", Obj.repr (order)); ("ele", Obj.repr (elements))]) : (string * Obj.t) list); raise Return
  with Return -> !__ret)

and unpackSym m =
  let __ret = ref ([] : float list list) in
  (try
  let n = (Obj.obj (List.assoc ("order") m) : int) in
  let ele = ref ((Obj.obj (List.assoc ("ele") m) : float list)) in
  let mat = ref (([] : float list list)) in
  let idx = ref (0) in
  let r = ref (0) in
  (try while (!r < n) do
    try
  let row = ref (([] : float list)) in
  let c = ref (0) in
  (try while (!c <= !r) do
    try
  row := (List.append (!row) [(Obj.magic (List.nth (!ele) (!idx)) : float)]);
  idx := (!idx + 1);
  c := (!c + 1);
    with Continue -> ()
  done with Break -> ());
  (try while (!c < n) do
    try
  row := (List.append (!row) [(Obj.magic (0.0) : float)]);
  c := (!c + 1);
    with Continue -> ()
  done with Break -> ());
  mat := (List.append (!mat) [(Obj.magic (!row) : float list)]);
  r := (!r + 1);
    with Continue -> ()
  done with Break -> ());
  r := 0;
  (try while (!r < n) do
    try
  let c = ref ((!r + 1)) in
  (try while (!c < n) do
    try
  mat := (List.mapi (fun __i __x -> if __i = !r then (List.mapi (fun __i __x -> if __i = !c then List.nth (List.nth (!mat) (!c)) (!r) else __x) (List.nth (!mat) (!r))) else __x) (!mat));
  c := (!c + 1);
    with Continue -> ()
  done with Break -> ());
  r := (!r + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!mat) : float list list); raise Return
  with Return -> !__ret)

and printMat m =
  let __ret = ref (Obj.magic 0) in
  (try
  let i = ref (0) in
  (try while (!i < List.length (m)) do
    try
  let line = ref ("") in
  let j = ref (0) in
  (try while (!j < List.length (List.nth (m) (!i))) do
    try
  line := (!line ^ (Printf.sprintf "%.16g" (List.nth (List.nth (m) (!i)) (!j))));
  if (!j < (List.length (List.nth (m) (!i)) - 1)) then (
  line := (!line ^ " ");
  );
  j := (!j + 1);
    with Continue -> ()
  done with Break -> ());
  print_endline (__show !line);
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
    !__ret
  with Return -> !__ret)

and printSym m =
  let __ret = ref (Obj.magic 0) in
  (try
  ignore (printMat (unpackSym (m)));
    !__ret
  with Return -> !__ret)

and printLower m =
  let __ret = ref (Obj.magic 0) in
  (try
  let n = (Obj.obj (List.assoc ("order") m) : int) in
  let ele = ref ((Obj.obj (List.assoc ("ele") m) : float list)) in
  let mat = ref (([] : float list list)) in
  let idx = ref (0) in
  let r = ref (0) in
  (try while (!r < n) do
    try
  let row = ref (([] : float list)) in
  let c = ref (0) in
  (try while (!c <= !r) do
    try
  row := (List.append (!row) [(Obj.magic (List.nth (!ele) (!idx)) : float)]);
  idx := (!idx + 1);
  c := (!c + 1);
    with Continue -> ()
  done with Break -> ());
  (try while (!c < n) do
    try
  row := (List.append (!row) [(Obj.magic (0.0) : float)]);
  c := (!c + 1);
    with Continue -> ()
  done with Break -> ());
  mat := (List.append (!mat) [(Obj.magic (!row) : float list)]);
  r := (!r + 1);
    with Continue -> ()
  done with Break -> ());
  ignore (printMat (!mat));
    !__ret
  with Return -> !__ret)

and choleskyLower a =
  let __ret = ref [] in
  (try
  let n = (Obj.obj (List.assoc ("order") a) : int) in
  let ae = ref ((Obj.obj (List.assoc ("ele") a) : float list)) in
  let le = ref (([] : float list)) in
  let idx = ref (0) in
  (try while (!idx < List.length (!ae)) do
    try
  le := (List.append (!le) [(Obj.magic (0.0) : float)]);
  idx := (!idx + 1);
    with Continue -> ()
  done with Break -> ());
  let row = ref (1) in
  let col = ref (1) in
  let dr = ref (0) in
  let dc = ref (0) in
  let i = ref (0) in
  (try while (!i < List.length (!ae)) do
    try
  let e = List.nth (!ae) (!i) in
  if (!i < !dr) then (
  let d = ref (((e -. List.nth (!le) (!i)) /. List.nth (!le) (!dc))) in
  le := (List.mapi (fun __i __x -> if __i = !i then !d else __x) (!le));
  let ci = ref (!col) in
  let cx = ref (!dc) in
  let j = ref ((!i + 1)) in
  (try while (!j <= !dr) do
    try
  cx := (!cx + !ci);
  ci := (!ci + 1);
  le := (List.mapi (fun __i __x -> if __i = !j then (List.nth (!le) (!j) +. (!d *. List.nth (!le) (!cx))) else __x) (!le));
  j := (!j + 1);
    with Continue -> ()
  done with Break -> ());
  col := (!col + 1);
  dc := (!dc + !col);
  ) else (
  le := (List.mapi (fun __i __x -> if __i = !i then sqrtApprox (Obj.repr ((e -. List.nth (!le) (!i)))) else __x) (!le));
  row := (!row + 1);
  dr := (!dr + !row);
  col := 1;
  dc := 0;
  );
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic ([("order", Obj.repr (n)); ("ele", Obj.repr (!le))]) : (string * Obj.t) list); raise Return
  with Return -> !__ret)

and demo a =
  let __ret = ref (Obj.magic 0) in
  (try
  print_endline ("A:");
  ignore (printSym (a));
  print_endline ("L:");
  let l = ref (choleskyLower (a)) in
  ignore (printLower (!l));
    !__ret
  with Return -> !__ret)


let () =
  let mem_start = _mem () in
  let start = _now () in
  ignore (demo (makeSym (Obj.repr (3)) ([25.0; 15.0; 18.0; (-.(5.0)); 0.0; 11.0])));
  ignore (demo (makeSym (Obj.repr (4)) ([18.0; 22.0; 70.0; 54.0; 86.0; 174.0; 42.0; 62.0; 134.0; 106.0])));
  let finish = _now () in
  let mem_end = _mem () in
  let dur = (finish - start) / 1000 in
  let mem_bytes = max 0 (mem_end - mem_start) in
  Printf.printf "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"%s\"\n}\n" dur mem_bytes "main";
  ()