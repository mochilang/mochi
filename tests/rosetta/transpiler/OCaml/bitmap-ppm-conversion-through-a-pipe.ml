(* Generated by Mochi transpiler v0.10.42 on 2025-07-27 22:15 +0700 *)


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

let rec pixelFromRgb c =
  let __ret = ref ([] : (string * Obj.t) list) in
  (try
  let c = (Obj.magic c : int) in
  let r = ((c / 65536) mod 256) in
  let g = ((c / 256) mod 256) in
  let b = (c mod 256) in
  __ret := (Obj.magic ([("R", Obj.repr r); ("G", Obj.repr g); ("B", Obj.repr b)]) : (string * Obj.t) list); raise Return
  with Return -> !__ret)

let rec rgbFromPixel p =
  let __ret = ref 0 in
  (try
  __ret := (Obj.magic (((((Obj.obj (List.assoc ("R") p) : int) * 65536) + ((Obj.obj (List.assoc ("G") p) : int) * 256)) + (Obj.obj (List.assoc ("B") p) : int))) : int); raise Return
  with Return -> !__ret)

let rec _newbitmap x y =
  let __ret = ref ([] : (string * Obj.t) list) in
  (try
  let x = (Obj.magic x : int) in
  let y = (Obj.magic y : int) in
  let data = ref ([]) in
  let row = ref (0) in
  (try while (!row < y) do
    try
  let r = ref ([]) in
  let col = ref (0) in
  (try while (!col < x) do
    try
  r := List.append !r [[("R", Obj.repr 0); ("G", Obj.repr 0); ("B", Obj.repr 0)]];
  col := (!col + 1);
    with Continue -> ()
  done with Break -> ());
  data := List.append !data [!r];
  row := (!row + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic ([("cols", Obj.repr x); ("rows", Obj.repr y); ("px", Obj.repr !data)]) : (string * Obj.t) list); raise Return
  with Return -> !__ret)

let rec _fillrgb b c =
  let __ret = ref (Obj.magic 0) in
  (try
  let c = (Obj.magic c : int) in
  let y = ref (0) in
  let p = pixelFromRgb (c) in
  (try while (!y < (Obj.obj (List.assoc ("rows") !b) : int)) do
    try
  let x = ref (0) in
  (try while (!x < (Obj.obj (List.assoc ("cols") !b) : int)) do
    try
  let px = ref ((Obj.obj (List.assoc ("px") !b) : (string * Obj.t) list list list)) in
  let row = ref (List.nth (!px) (!y)) in
  row := (List.mapi (fun __i __x -> if __i = !x then p else __x) (!row));
  px := (List.mapi (fun __i __x -> if __i = !y then !row else __x) (!px));
  b := (("px", Obj.repr !px) :: List.remove_assoc "px" (Obj.magic (!b) : (string * Obj.t) list));
  x := (!x + 1);
    with Continue -> ()
  done with Break -> ());
  y := (!y + 1);
    with Continue -> ()
  done with Break -> ());
    !__ret
  with Return -> !__ret)

let rec _setpxrgb b x y c =
  let __ret = ref false in
  (try
  let x = (Obj.magic x : int) in
  let y = (Obj.magic y : int) in
  let c = (Obj.magic c : int) in
  if ((((x < 0) || (x >= (Obj.obj (List.assoc ("cols") !b) : int))) || (y < 0)) || (y >= (Obj.obj (List.assoc ("rows") !b) : int))) then (
  __ret := (Obj.magic (false) : bool); raise Return
  );
  let px = ref ((Obj.obj (List.assoc ("px") !b) : (string * Obj.t) list list list)) in
  let row = ref (List.nth (!px) (y)) in
  row := (List.mapi (fun __i __x -> if __i = x then pixelFromRgb (c) else __x) (!row));
  px := (List.mapi (fun __i __x -> if __i = y then !row else __x) (!px));
  b := (("px", Obj.repr !px) :: List.remove_assoc "px" (Obj.magic (!b) : (string * Obj.t) list));
  __ret := (Obj.magic (true) : bool); raise Return
  with Return -> !__ret)

let rec nextRand seed =
  let __ret = ref 0 in
  (try
  let seed = (Obj.magic seed : int) in
  __ret := (Obj.magic ((((seed * 1664525) + 1013904223) mod 2147483648)) : int); raise Return
  with Return -> !__ret)

let rec main () =
  let __ret = ref (Obj.magic 0) in
  (try
  let bm = ref (_newbitmap (400) (300)) in
  ignore (_fillrgb (bm) (12615744));
  let seed = ref (_now ()) in
  let i = ref (0) in
  (try while (!i < 2000) do
    try
  seed := nextRand (!seed);
  let x = (!seed mod 400) in
  seed := nextRand (!seed);
  let y = (!seed mod 300) in
  ignore (_setpxrgb (bm) (x) (y) (8405024));
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  let x = ref (0) in
  (try while (!x < 400) do
    try
  let y = ref (240) in
  (try while (!y < 245) do
    try
  ignore (_setpxrgb (bm) (!x) (!y) (8405024));
  y := (!y + 1);
    with Continue -> ()
  done with Break -> ());
  y := 260;
  (try while (!y < 265) do
    try
  ignore (_setpxrgb (bm) (!x) (!y) (8405024));
  y := (!y + 1);
    with Continue -> ()
  done with Break -> ());
  x := (!x + 1);
    with Continue -> ()
  done with Break -> ());
  let y = ref (0) in
  (try while (!y < 300) do
    try
  let x = ref (80) in
  (try while (!x < 85) do
    try
  ignore (_setpxrgb (bm) (!x) (!y) (8405024));
  x := (!x + 1);
    with Continue -> ()
  done with Break -> ());
  x := 95;
  (try while (!x < 100) do
    try
  ignore (_setpxrgb (bm) (!x) (!y) (8405024));
  x := (!x + 1);
    with Continue -> ()
  done with Break -> ());
  y := (!y + 1);
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
