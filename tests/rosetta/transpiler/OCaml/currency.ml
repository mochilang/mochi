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

let rec parseIntDigits s =
  let __ret = ref 0 in
  (try
  let n = ref (0) in
  let i = ref (0) in
  let digits = ref ([("0", 0); ("1", 1); ("2", 2); ("3", 3); ("4", 4); ("5", 5); ("6", 6); ("7", 7); ("8", 8); ("9", 9)]) in
  (try while (!i < String.length (s)) do
    try
  let ch = String.sub (s) !i ((!i + 1) - !i) in
  if not ((List.mem_assoc ch !digits)) then (
  __ret := (Obj.magic (0) : int); raise Return
  );
  n := ((!n * 10) + (try List.assoc (ch) !digits with Not_found -> (Obj.magic 0)));
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!n) : int); raise Return
  with Return -> !__ret)

and parseDC s =
  let __ret = ref 0 in
  (try
  let neg = ref (false) in
  if ((String.length (!s) > 0) && (String.sub (!s) 0 (1 - 0) = "-")) then (
  neg := true;
  s := String.sub (!s) 1 (String.length (!s) - 1);
  );
  let dollars = ref (0) in
  let cents = ref (0) in
  let i = ref (0) in
  let seenDot = ref (false) in
  let centDigits = ref (0) in
  (try while (!i < String.length (!s)) do
    try
  let ch = String.sub (!s) !i ((!i + 1) - !i) in
  if (ch = ".") then (
  seenDot := true;
  i := (!i + 1);
  raise Continue;
  );
  let d = parseIntDigits (ch) in
  if !seenDot then (
  if (!centDigits < 2) then (
  cents := ((!cents * 10) + d);
  centDigits := (!centDigits + 1);
  );
  ) else (
  dollars := ((!dollars * 10) + d);
  );
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  if (!centDigits = 1) then (
  cents := (!cents * 10);
  );
  let val_ = ref (((!dollars * 100) + !cents)) in
  if !neg then (
  val_ := -(!val_);
  );
  __ret := (Obj.magic (!val_) : int); raise Return
  with Return -> !__ret)

and parseRate s =
  let __ret = ref 0 in
  (try
  let neg = ref (false) in
  if ((String.length (!s) > 0) && (String.sub (!s) 0 (1 - 0) = "-")) then (
  neg := true;
  s := String.sub (!s) 1 (String.length (!s) - 1);
  );
  let whole = ref (0) in
  let frac = ref (0) in
  let digits = ref (0) in
  let seenDot = ref (false) in
  let i = ref (0) in
  (try while (!i < String.length (!s)) do
    try
  let ch = String.sub (!s) !i ((!i + 1) - !i) in
  if (ch = ".") then (
  seenDot := true;
  i := (!i + 1);
  raise Continue;
  );
  let d = parseIntDigits (ch) in
  if !seenDot then (
  if (!digits < 4) then (
  frac := ((!frac * 10) + d);
  digits := (!digits + 1);
  );
  ) else (
  whole := ((!whole * 10) + d);
  );
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  (try while (!digits < 4) do
    try
  frac := (!frac * 10);
  digits := (!digits + 1);
    with Continue -> ()
  done with Break -> ());
  let val_ = ref (((!whole * 10000) + !frac)) in
  if !neg then (
  val_ := -(!val_);
  );
  __ret := (Obj.magic (!val_) : int); raise Return
  with Return -> !__ret)

and dcString dc =
  let __ret = ref "" in
  (try
  let dc = (Obj.magic dc : int) in
  let d = ref ((dc / 100)) in
  let n = ref (dc) in
  if (!n < 0) then (
  n := -(!n);
  );
  let c = ref ((!n mod 100)) in
  let cstr = ref ((string_of_int (!c))) in
  if (String.length (!cstr) = 1) then (
  cstr := ("0" ^ !cstr);
  );
  __ret := (Obj.magic ((((string_of_int (!d)) ^ ".") ^ !cstr)) : string); raise Return
  with Return -> !__ret)

and extend dc n =
  let __ret = ref 0 in
  (try
  let dc = (Obj.magic dc : int) in
  let n = (Obj.magic n : int) in
  __ret := (Obj.magic ((dc * n)) : int); raise Return
  with Return -> !__ret)

and tax total rate =
  let __ret = ref 0 in
  (try
  let total = (Obj.magic total : int) in
  let rate = (Obj.magic rate : int) in
  __ret := (Obj.magic ((((total * rate) + 5000) / 10000)) : int); raise Return
  with Return -> !__ret)

and padLeft s n =
  let __ret = ref "" in
  (try
  let n = (Obj.magic n : int) in
  let out = ref (s) in
  (try while (String.length (!out) < n) do
    try
  out := (" " ^ !out);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!out) : string); raise Return
  with Return -> !__ret)

and main () =
  let __ret = ref (Obj.magic 0) in
  (try
  let hp = parseDC (ref ("5.50")) in
  let mp = parseDC (ref ("2.86")) in
  let rate = parseRate (ref ("0.0765")) in
  let totalBeforeTax = (extend (Obj.repr (hp)) (Obj.repr (4000000000000000)) + extend (Obj.repr (mp)) (Obj.repr (2))) in
  let t = tax (Obj.repr (totalBeforeTax)) (Obj.repr (rate)) in
  let total = (totalBeforeTax + t) in
  print_endline (("Total before tax: " ^ padLeft (dcString (Obj.repr (totalBeforeTax))) (Obj.repr (22))));
  print_endline (("             Tax: " ^ padLeft (dcString (Obj.repr (t))) (Obj.repr (22))));
  print_endline (("           Total: " ^ padLeft (dcString (Obj.repr (total))) (Obj.repr (22))));
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