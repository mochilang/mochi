(* Generated by Mochi transpiler v0.10.40 on 2025-07-25 20:48 +0700 *)


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

let rec neighborsList () =
  let __ret = ref [] in
  (try
  __ret := [[1; 3]; [0; 2; 4]; [1; 5]; [0; 4; 6]; [1; 3; 5; 7]; [2; 4; 8]; [3; 7]; [4; 6; 8]; [5; 7]]; raise Return
  with Return -> !__ret)

let rec plus a b =
  let __ret = ref [] in
  (try
  let res = ref ([]) in
  let i = ref (0) in
  (try while (!i < List.length (a)) do
    try
  res := List.append !res [(List.nth (a) (!i) + List.nth (b) (!i))];
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := !res; raise Return
  with Return -> !__ret)

let rec isStable p =
  let __ret = ref false in
  (try
  (try List.iter (fun v ->
    try
  if (v > 3) then (
  __ret := false; raise Return
  );
    with Continue -> ()) p with Break -> ());
  __ret := true; raise Return
  with Return -> !__ret)

let rec topple p =
  let __ret = ref 0 in
  let p = ref p in
  (try
  let neighbors = ref (neighborsList ()) in
  let i = ref (0) in
  (try while (!i < List.length (!p)) do
    try
  if (List.nth (!p) (!i) > 3) then (
  p := (List.mapi (fun __i __x -> if __i = !i then (List.nth (!p) (!i) - 4) else __x) (!p));
  let nbs = ref (List.nth (!neighbors) (!i)) in
  (try List.iter (fun j ->
    try
  p := (List.mapi (fun __i __x -> if __i = j then (List.nth (!p) (j) + 1) else __x) (!p));
    with Continue -> ()) !nbs with Break -> ());
  __ret := 0; raise Return
  );
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := 0; raise Return
  with Return -> !__ret)

let rec pileString p =
  let __ret = ref "" in
  (try
  let s = ref ("") in
  let r = ref (0) in
  (try while (!r < 3) do
    try
  let c = ref (0) in
  (try while (!c < 3) do
    try
  s := ((!s ^ (string_of_int (List.nth (p) (((3 * !r) + !c))))) ^ " ");
  c := (!c + 1);
    with Continue -> ()
  done with Break -> ());
  s := (!s ^ "\n");
  r := (!r + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := !s; raise Return
  with Return -> !__ret)

let s4 = ref ([4; 3; 3; 3; 1; 2; 0; 2; 3])
let s1 = ref ([1; 2; 0; 2; 1; 1; 0; 1; 3])
let s2 = ref ([2; 1; 3; 1; 0; 1; 0; 1; 0])
let s3_a = ref (plus (!s1) (!s2))
let s3_b = ref (plus (!s2) (!s1))
let s3 = ref ([3; 3; 3; 3; 3; 3; 3; 3; 3])
let s3_id = ref ([2; 1; 2; 1; 0; 1; 2; 1; 2])
let s4b = ref (plus (!s3) (!s3_id))
let s5 = ref (plus (!s3_id) (!s3_id))
let () =
  let mem_start = _mem () in
  let start = _now () in
  print_endline ("Avalanche of topplings:\n");
  print_endline (pileString (!s4));
  (try while not (isStable (!s4)) do
    try
  ignore (topple (!s4));
  print_endline (pileString (!s4));
    with Continue -> ()
  done with Break -> ());
  print_endline ("Commutative additions:\n");
  (try while not (isStable (!s3_a)) do
    try
  ignore (topple (!s3_a));
    with Continue -> ()
  done with Break -> ());
  (try while not (isStable (!s3_b)) do
    try
  ignore (topple (!s3_b));
    with Continue -> ()
  done with Break -> ());
  print_endline (((((pileString (!s1) ^ "\nplus\n\n") ^ pileString (!s2)) ^ "\nequals\n\n") ^ pileString (!s3_a)));
  print_endline (((((("and\n\n" ^ pileString (!s2)) ^ "\nplus\n\n") ^ pileString (!s1)) ^ "\nalso equals\n\n") ^ pileString (!s3_b)));
  print_endline ("Addition of identity sandpile:\n");
  (try while not (isStable (!s4b)) do
    try
  ignore (topple (!s4b));
    with Continue -> ()
  done with Break -> ());
  print_endline (((((pileString (!s3) ^ "\nplus\n\n") ^ pileString (!s3_id)) ^ "\nequals\n\n") ^ pileString (!s4b)));
  print_endline ("Addition of identities:\n");
  (try while not (isStable (!s5)) do
    try
  ignore (topple (!s5));
    with Continue -> ()
  done with Break -> ());
  print_endline (((((pileString (!s3_id) ^ "\nplus\n\n") ^ pileString (!s3_id)) ^ "\nequals\n\n") ^ pileString (!s5)));
  let finish = _now () in
  let mem_end = _mem () in
  let dur = (finish - start) / 1000 in
  let mem_bytes = max 0 (mem_end - mem_start) in
  Printf.printf "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"%s\"\n}\n" dur mem_bytes "main";
