(* Generated by Mochi transpiler v0.10.41 on 2025-07-26 19:01 +0700 *)


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

let rec applyFilter input a b =
  let __ret = ref ([] : float list) in
  (try
  let out = ref ([]) in
  let scale = (1.0 /. List.nth (a) (0)) in
  let i = ref (0) in
  (try while (!i < List.length (input)) do
    try
  let tmp = ref (0.0) in
  let j = ref (0) in
  (try while ((!j <= !i) && (!j < List.length (b))) do
    try
  tmp := (!tmp +. (List.nth (b) (!j) *. List.nth (input) ((!i - !j))));
  j := (!j + 1);
    with Continue -> ()
  done with Break -> ());
  j := 0;
  (try while ((!j < !i) && ((!j + 1) < List.length (a))) do
    try
  tmp := (!tmp -. (List.nth (a) ((!j + 1)) *. List.nth (!out) (((!i - !j) - 1))));
  j := (!j + 1);
    with Continue -> ()
  done with Break -> ());
  out := List.append !out [(!tmp *. scale)];
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!out) : float list); raise Return
  with Return -> !__ret)

let a = ref ([1.0; (-.(0.00000000000000027756)); 0.33333333; (-.(0.0000000000000000185))])
let b = ref ([0.16666667; 0.5; 0.5; 0.16666667])
let sig_ = ref ([(-.(0.917843918645)); 0.141984778794; 1.20536903482; 0.190286794412; (-.(0.662370894973)); (-.(1.00700480494)); (-.(0.404707073677)); 0.800482325044; 0.743500089861; 1.01090520172; 0.741527555207; 0.277841675195; 0.400833448236; (-.(0.2085993586)); (-.(0.172842103641)); (-.(0.134316096293)); 0.0259303398477; 0.490105989562; 0.549391221511; 0.9047198589])
let res = ref (applyFilter (!sig_) (!a) (!b))
let k = ref (0)
let () =
  let mem_start = _mem () in
  let start = _now () in
  (try while (!k < List.length (!res)) do
    try
  print_endline (Printf.sprintf "%.15f" (List.nth (!res) (!k)));
  k := (!k + 1);
    with Continue -> ()
  done with Break -> ());
  let finish = _now () in
  let mem_end = _mem () in
  let dur = (finish - start) / 1000 in
  let mem_bytes = max 0 (mem_end - mem_start) in
  Printf.printf "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"%s\"\n}\n" dur mem_bytes "main";
