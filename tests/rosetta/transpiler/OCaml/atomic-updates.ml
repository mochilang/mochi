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

let rec randOrder seed n =
  let __ret = ref ([] : int list) in
  (try
  let seed = (Obj.magic seed : int) in
  let n = (Obj.magic n : int) in
  let next = (((seed * 1664525) + 1013904223) mod 2147483647) in
  __ret := (Obj.magic ([next; (next mod n)]) : int list); raise Return
  with Return -> !__ret)

let rec randChaos seed n =
  let __ret = ref ([] : int list) in
  (try
  let seed = (Obj.magic seed : int) in
  let n = (Obj.magic n : int) in
  let next = (((seed * 1103515245) + 12345) mod 2147483647) in
  __ret := (Obj.magic ([next; (next mod n)]) : int list); raise Return
  with Return -> !__ret)

let rec main () =
  let __ret = ref (Obj.magic 0) in
  (try
  let nBuckets = 10 in
  let initialSum = 1000 in
  let buckets = ref ([]) in
  (try for i = 0 to (nBuckets - 1) do
    try
  buckets := List.append !buckets [0];
    with Continue -> ()
  done with Break -> ());
  let i = ref (nBuckets) in
  let dist = ref (initialSum) in
  (try while (!i > 0) do
    try
  let v = (!dist / !i) in
  i := (!i - 1);
  buckets := (List.mapi (fun __i __x -> if __i = !i then v else __x) (!buckets));
  dist := (!dist - v);
    with Continue -> ()
  done with Break -> ());
  let tc0 = ref (0) in
  let tc1 = ref (0) in
  let total = ref (0) in
  let nTicks = ref (0) in
  let seedOrder = ref (1) in
  let seedChaos = ref (2) in
  print_endline ("sum  ---updates---    mean  buckets");
  let t = ref (0) in
  (try while (!t < 5) do
    try
  let r = ref (randOrder (!seedOrder) (nBuckets)) in
  seedOrder := List.nth (!r) (0);
  let b1 = ref (List.nth (!r) (1)) in
  let b2 = ref (((!b1 + 1) mod nBuckets)) in
  let v1 = List.nth (!buckets) (!b1) in
  let v2 = List.nth (!buckets) (!b2) in
  if (v1 > v2) then (
  let a = ref (((v1 - v2) / 2)) in
  if (!a > List.nth (!buckets) (!b1)) then (
  a := List.nth (!buckets) (!b1);
  );
  buckets := (List.mapi (fun __i __x -> if __i = !b1 then (List.nth (!buckets) (!b1) - !a) else __x) (!buckets));
  buckets := (List.mapi (fun __i __x -> if __i = !b2 then (List.nth (!buckets) (!b2) + !a) else __x) (!buckets));
  ) else (
  let a = ref (((v2 - v1) / 2)) in
  if (!a > List.nth (!buckets) (!b2)) then (
  a := List.nth (!buckets) (!b2);
  );
  buckets := (List.mapi (fun __i __x -> if __i = !b2 then (List.nth (!buckets) (!b2) - !a) else __x) (!buckets));
  buckets := (List.mapi (fun __i __x -> if __i = !b1 then (List.nth (!buckets) (!b1) + !a) else __x) (!buckets));
  );
  tc0 := (!tc0 + 1);
  r := randChaos (!seedChaos) (nBuckets);
  seedChaos := List.nth (!r) (0);
  b1 := List.nth (!r) (1);
  b2 := ((!b1 + 1) mod nBuckets);
  r := randChaos (!seedChaos) ((List.nth (!buckets) (!b1) + 1));
  seedChaos := List.nth (!r) (0);
  let amt = ref (List.nth (!r) (1)) in
  if (!amt > List.nth (!buckets) (!b1)) then (
  amt := List.nth (!buckets) (!b1);
  );
  buckets := (List.mapi (fun __i __x -> if __i = !b1 then (List.nth (!buckets) (!b1) - !amt) else __x) (!buckets));
  buckets := (List.mapi (fun __i __x -> if __i = !b2 then (List.nth (!buckets) (!b2) + !amt) else __x) (!buckets));
  tc1 := (!tc1 + 1);
  let sum = ref (0) in
  let idx = ref (0) in
  (try while (!idx < nBuckets) do
    try
  sum := (!sum + List.nth (!buckets) (!idx));
  idx := (!idx + 1);
    with Continue -> ()
  done with Break -> ());
  total := ((!total + !tc0) + !tc1);
  nTicks := (!nTicks + 1);
  print_endline ((((((((((string_of_int (!sum)) ^ " ") ^ (string_of_int (!tc0))) ^ " ") ^ (string_of_int (!tc1))) ^ " ") ^ (string_of_int ((!total / !nTicks)))) ^ "  ") ^ __show (!buckets)));
  tc0 := 0;
  tc1 := 0;
  t := (!t + 1);
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
