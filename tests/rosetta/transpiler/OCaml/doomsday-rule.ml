(* Generated by Mochi transpiler v0.10.55 on 2025-08-02 18:27 +0700 *)


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
  if is_int r then
    let i = (magic v : int) in
    if i = 0 || i = 1 then string_of_bool (i <> 0)
    else string_of_int i
  else
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

let rec parseIntStr str =
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
  n := ((!n * 10) + (try List.assoc (String.sub (str) !i ((!i + 1) - !i)) !digits with Not_found -> 0));
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  if !neg then (
  n := -(!n);
  );
  __ret := (Obj.magic (!n) : int); raise Return
  with Return -> !__ret)


let days = ref (["Sunday"; "Monday"; "Tuesday"; "Wednesday"; "Thursday"; "Friday"; "Saturday"])
let rec anchorDay y =
  let __ret = ref 0 in
  (try
  let y = (Obj.magic y : int) in
  __ret := (Obj.magic (((((2 + (5 * (y mod 4))) + (4 * (y mod 100))) + (6 * (y mod 400))) mod 7)) : int); raise Return
  with Return -> !__ret)

and isLeapYear y =
  let __ret = ref false in
  (try
  let y = (Obj.magic y : int) in
  __ret := (Obj.magic ((((y mod 4) = 0) && (((y mod 100) <> 0) || ((y mod 400) = 0)))) : bool); raise Return
  with Return -> !__ret)


let firstDaysCommon = ref ([3; 7; 7; 4; 2; 6; 4; 1; 5; 3; 7; 5])
let firstDaysLeap = ref ([4; 1; 7; 4; 2; 6; 4; 1; 5; 3; 7; 5])
let rec main () =
  let __ret = ref (Obj.magic 0) in
  (try
  let dates = ref (["1800-01-06"; "1875-03-29"; "1915-12-07"; "1970-12-23"; "2043-05-14"; "2077-02-12"; "2101-04-02"]) in
  print_endline ("Days of week given by Doomsday rule:");
  (try List.iter (fun date ->
    try
  let y = parseIntStr (String.sub (date) 0 (4 - 0)) in
  let m = (parseIntStr (String.sub (date) 5 (7 - 5)) - 1) in
  let d = parseIntStr (String.sub (date) 8 (10 - 8)) in
  let a = anchorDay (Obj.repr (y)) in
  let f = ref (List.nth (!firstDaysCommon) (m)) in
  if isLeapYear (Obj.repr (y)) then (
  f := List.nth (!firstDaysLeap) (m);
  );
  let w = ref ((d - !f)) in
  if (!w < 0) then (
  w := (7 + !w);
  );
  let dow = ((a + !w) mod 7) in
  print_endline (((date ^ " -> ") ^ List.nth (!days) (dow)));
    with Continue -> ()) (!dates) with Break -> ());
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