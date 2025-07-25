(* Generated by Mochi transpiler v0.10.40 on 2025-07-26 00:13 +0700 *)


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

let rec split s sep =
  let __ret = ref ([] : string list) in
  (try
  let parts = ref ([]) in
  let cur = ref ("") in
  let i = ref (0) in
  (try while (!i < String.length (s)) do
    try
  if (((String.length (sep) > 0) && ((!i + String.length (sep)) <= String.length (s))) && (String.sub (s) !i ((!i + String.length (sep)) - !i) = sep)) then (
  parts := List.append !parts [!cur];
  cur := "";
  i := (!i + String.length (sep));
  ) else (
  cur := (!cur ^ String.sub (s) !i ((!i + 1) - !i));
  i := (!i + 1);
  );
    with Continue -> ()
  done with Break -> ());
  parts := List.append !parts [!cur];
  __ret := (Obj.magic (!parts) : string list); raise Return
  with Return -> !__ret)

let rec rstripEmpty words =
  let __ret = ref ([] : string list) in
  (try
  let n = ref (List.length (words)) in
  (try while ((!n > 0) && (List.nth (words) ((!n - 1)) = "")) do
    try
  n := (!n - 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (List.of_seq (Seq.take (!n - 0) (Seq.drop 0 (List.to_seq words)))) : string list); raise Return
  with Return -> !__ret)

let rec spaces n =
  let __ret = ref "" in
  (try
  let n = (Obj.magic n : int) in
  let out = ref ("") in
  let i = ref (0) in
  (try while (!i < n) do
    try
  out := (!out ^ " ");
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!out) : string); raise Return
  with Return -> !__ret)

let rec pad word width align =
  let __ret = ref "" in
  (try
  let width = (Obj.magic width : int) in
  let align = (Obj.magic align : int) in
  let diff = (width - String.length (word)) in
  if (align = 0) then (
  __ret := (Obj.magic ((word ^ spaces (diff))) : string); raise Return
  );
  if (align = 2) then (
  __ret := (Obj.magic ((spaces (diff) ^ word)) : string); raise Return
  );
  let left = ref ((diff / 2)) in
  let right = ref ((diff - !left)) in
  __ret := (Obj.magic (((spaces (!left) ^ word) ^ spaces (!right))) : string); raise Return
  with Return -> !__ret)

let rec newFormatter text =
  let __ret = ref [] in
  (try
  let lines = ref (split (text) ("\n")) in
  let fmtLines = ref ([]) in
  let width = ref ([]) in
  let i = ref (0) in
  (try while (!i < List.length (!lines)) do
    try
  if (String.length (List.nth (!lines) (!i)) = 0) then (
  i := (!i + 1);
  raise Continue;
  );
  let words = ref (rstripEmpty (split (List.nth (!lines) (!i)) ("$"))) in
  fmtLines := List.append !fmtLines [!words];
  let j = ref (0) in
  (try while (!j < List.length (!words)) do
    try
  let wlen = String.length (List.nth (!words) (!j)) in
  if (!j = List.length (!width)) then (
  width := List.append !width [wlen];
  ) else (
  if (wlen > List.nth (!width) (!j)) then (
  width := (List.mapi (fun __i __x -> if __i = !j then wlen else __x) (!width));
  );
  );
  j := (!j + 1);
    with Continue -> ()
  done with Break -> ());
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic ([("text", Obj.repr !fmtLines); ("width", Obj.repr !width)]) : (string * Obj.t) list); raise Return
  with Return -> !__ret)

let rec printFmt f align =
  let __ret = ref (Obj.magic 0) in
  (try
  let align = (Obj.magic align : int) in
  let lines = ref ((Obj.obj (List.assoc ("text") f) : string list list)) in
  let width = ref ((Obj.obj (List.assoc ("width") f) : int list)) in
  let i = ref (0) in
  (try while (!i < List.length (!lines)) do
    try
  let words = ref (List.nth (!lines) (!i)) in
  let line = ref ("") in
  let j = ref (0) in
  (try while (!j < List.length (!words)) do
    try
  line := ((!line ^ pad (List.nth (!words) (!j)) (List.nth (!width) (!j)) (align)) ^ " ");
  j := (!j + 1);
    with Continue -> ()
  done with Break -> ());
  print_endline (__show !line);
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  print_endline ("");
    !__ret
  with Return -> !__ret)

let text = ((((("Given$a$text$file$of$many$lines,$where$fields$within$a$line\n" ^ "are$delineated$by$a$single$'dollar'$character,$write$a$program\n") ^ "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each\n") ^ "column$are$separated$by$at$least$one$space.\n") ^ "Further,$allow$for$each$word$in$a$column$to$be$either$left\n") ^ "justified,$right$justified,$or$center$justified$within$its$column.")
let f = ref (newFormatter (text))
let () =
  let mem_start = _mem () in
  let start = _now () in
  ignore (printFmt (!f) (0));
  ignore (printFmt (!f) (1));
  ignore (printFmt (!f) (2));
  let finish = _now () in
  let mem_end = _mem () in
  let dur = (finish - start) / 1000 in
  let mem_bytes = max 0 (mem_end - mem_start) in
  Printf.printf "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"%s\"\n}\n" dur mem_bytes "main";
