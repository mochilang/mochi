(* Generated by Mochi transpiler v0.10.52 on 2025-08-01 18:57 +0700 *)


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

let rec chr n =
  let __ret = ref "" in
  (try
  let n = (Obj.magic n : int) in
  let upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
  let lower = "abcdefghijklmnopqrstuvwxyz" in
  if ((n >= 65) && (n < 91)) then (
  __ret := (Obj.magic (String.sub (upper) (n - 65) ((n - 64) - (n - 65))) : string); raise Return
  );
  if ((n >= 97) && (n < 123)) then (
  __ret := (Obj.magic (String.sub (lower) (n - 97) ((n - 96) - (n - 97))) : string); raise Return
  );
  if (n = 32) then (
  __ret := (Obj.magic (" ") : string); raise Return
  );
  if (n = 33) then (
  __ret := (Obj.magic ("!") : string); raise Return
  );
  if (n = 44) then (
  __ret := (Obj.magic (",") : string); raise Return
  );
  if (n = 13) then (
  __ret := (Obj.magic ("") : string); raise Return
  );
  if (n = 10) then (
  __ret := (Obj.magic ("\n") : string); raise Return
  );
  __ret := (Obj.magic ("?") : string); raise Return
  with Return -> !__ret)

and bf dLen code =
  let __ret = ref "" in
  (try
  let dLen = (Obj.magic dLen : int) in
  let ds = ref (([] : int list)) in
  (try for i = 0 to (dLen - 1) do
    try
  ds := (List.append (!ds) [(Obj.magic (0) : int)]);
    with Continue -> ()
  done with Break -> ());
  let dp = ref (0) in
  let ip = ref (0) in
  let out = ref ("") in
  (try while (!ip < String.length (code)) do
    try
  let ch = String.sub (code) !ip ((!ip + 1) - !ip) in
  if (ch = ">") then (
  dp := (!dp + 1);
  ) else (
  if (ch = "<") then (
  dp := (!dp - 1);
  ) else (
  if (ch = "+") then (
  ds := (List.mapi (fun __i __x -> if __i = !dp then (List.nth (!ds) (!dp) + 1) else __x) (!ds));
  ) else (
  if (ch = "-") then (
  ds := (List.mapi (fun __i __x -> if __i = !dp then (List.nth (!ds) (!dp) - 1) else __x) (!ds));
  ) else (
  if (ch = ".") then (
  out := (!out ^ chr (List.nth (!ds) (!dp)));
  ) else (
  if (ch = ",") then (
  ) else (
  if (ch = "[") then (
  if (List.nth (!ds) (!dp) = 0) then (
  let nc = ref (1) in
  (try while (!nc > 0) do
    try
  ip := (!ip + 1);
  let cc = String.sub (code) !ip ((!ip + 1) - !ip) in
  if (cc = "[") then (
  nc := (!nc + 1);
  ) else (
  if (cc = "]") then (
  nc := (!nc - 1);
  );
  );
    with Continue -> ()
  done with Break -> ());
  );
  ) else (
  if (ch = "]") then (
  if (List.nth (!ds) (!dp) <> 0) then (
  let nc = ref (1) in
  (try while (!nc > 0) do
    try
  ip := (!ip - 1);
  let cc = String.sub (code) !ip ((!ip + 1) - !ip) in
  if (cc = "]") then (
  nc := (!nc + 1);
  ) else (
  if (cc = "[") then (
  nc := (!nc - 1);
  );
  );
    with Continue -> ()
  done with Break -> ());
  );
  );
  );
  );
  );
  );
  );
  );
  );
  ip := (!ip + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!out) : string); raise Return
  with Return -> !__ret)

and main () =
  let __ret = ref (Obj.magic 0) in
  (try
  let prog = ((("++++++++++[>+>+++>++++>+++++++>++++++++>+++++++++>++\n" ^ "++++++++>+++++++++++>++++++++++++<<<<<<<<<-]>>>>+.>>>\n") ^ ">+..<.<++++++++.>>>+.<<+.<<<<++++.<++.>>>+++++++.>>>.+++.\n") ^ "<+++++++.--------.<<<<<+.<+++.---.") in
  let out = bf (10) (prog) in
  print_endline (__show out);
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