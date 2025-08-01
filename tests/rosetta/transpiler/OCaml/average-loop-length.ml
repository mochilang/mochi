(* Generated by Mochi transpiler v0.10.42 on 2025-07-27 17:23 +0700 *)


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

let rec absf x =
  let __ret = ref 0.0 in
  (try
  let x = (Obj.magic x : float) in
  if (x < 0.0) then (
  __ret := (Obj.magic ((-.(x))) : float); raise Return
  );
  __ret := (Obj.magic (x) : float); raise Return
  with Return -> !__ret)

let rec floorf x =
  let __ret = ref 0.0 in
  (try
  let x = (Obj.magic x : float) in
  let y = (Obj.magic x : int) in
  __ret := (Obj.magic (float_of_int (y)) : float); raise Return
  with Return -> !__ret)

let rec indexOf s ch =
  let __ret = ref 0 in
  (try
  let i = ref (0) in
  (try while (!i < String.length (s)) do
    try
  if (String.sub (s) !i ((!i + 1) - !i) = ch) then (
  __ret := (Obj.magic (!i) : int); raise Return
  );
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (-(1)) : int); raise Return
  with Return -> !__ret)

let rec fmtF x =
  let __ret = ref "" in
  (try
  let x = (Obj.magic x : float) in
  let y = ref ((floorf (((x *. 10000.0) +. 0.5)) /. 10000.0)) in
  let s = ref ((string_of_float (!y))) in
  let dot = ref (indexOf (!s) (".")) in
  if (!dot = (0 - 1)) then (
  s := (!s ^ ".0000");
  ) else (
  let decs = ref (((String.length (!s) - !dot) - 1)) in
  if (!decs > 4) then (
  s := String.sub (!s) 0 ((!dot + 5) - 0);
  ) else (
  (try while (!decs < 4) do
    try
  s := (!s ^ "0");
  decs := (!decs + 1);
    with Continue -> ()
  done with Break -> ());
  );
  );
  __ret := (Obj.magic (!s) : string); raise Return
  with Return -> !__ret)

let rec padInt n width =
  let __ret = ref "" in
  (try
  let n = (Obj.magic n : int) in
  let width = (Obj.magic width : int) in
  let s = ref ((string_of_int (n))) in
  (try while (String.length (!s) < width) do
    try
  s := (" " ^ !s);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!s) : string); raise Return
  with Return -> !__ret)

let rec padFloat x width =
  let __ret = ref "" in
  (try
  let x = (Obj.magic x : float) in
  let width = (Obj.magic width : int) in
  let s = ref (fmtF (x)) in
  (try while (String.length (!s) < width) do
    try
  s := (" " ^ !s);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!s) : string); raise Return
  with Return -> !__ret)

let rec avgLen n =
  let __ret = ref 0.0 in
  (try
  let n = (Obj.magic n : int) in
  let tests = 10000 in
  let sum = ref (0) in
  let seed = ref (1) in
  let t = ref (0) in
  (try while (!t < tests) do
    try
  let visited = ref ([]) in
  let i = ref (0) in
  (try while (!i < n) do
    try
  visited := List.append !visited [false];
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  let x = ref (0) in
  (try while not (List.nth (!visited) (!x)) do
    try
  visited := (List.mapi (fun __i __x -> if __i = !x then true else __x) (!visited));
  sum := (!sum + 1);
  seed := (((!seed * 1664525) + 1013904223) mod 2147483647);
  x := (!seed mod n);
    with Continue -> ()
  done with Break -> ());
  t := (!t + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic ((float_of_int (!sum) /. float_of_int (tests))) : float); raise Return
  with Return -> !__ret)

let rec ana n =
  let __ret = ref 0.0 in
  (try
  let n = (Obj.magic n : int) in
  let nn = ref (float_of_int (n)) in
  let term = ref (1.0) in
  let sum = ref (1.0) in
  let i = ref ((!nn -. 1.0)) in
  (try while (!i >= 1.0) do
    try
  term := (!term *. (!i /. !nn));
  sum := (!sum +. !term);
  i := (!i -. 1.0);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!sum) : float); raise Return
  with Return -> !__ret)

let rec main () =
  let __ret = ref (Obj.magic 0) in
  (try
  let nmax = 20 in
  print_endline (" N    average    analytical    (error)");
  print_endline ("===  =========  ============  =========");
  let n = ref (1) in
  (try while (!n <= nmax) do
    try
  let a = avgLen (!n) in
  let b = ana (!n) in
  let err = ((absf ((a -. b)) /. b) *. 100.0) in
  let line = ref ((((((((padInt (!n) (3) ^ "  ") ^ padFloat (a) (9)) ^ "  ") ^ padFloat (b) (12)) ^ "  (") ^ padFloat (err) (6)) ^ "%)")) in
  print_endline (__show !line);
  n := (!n + 1);
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
