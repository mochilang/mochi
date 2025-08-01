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

let rec commatize n =
  let __ret = ref "" in
  (try
  let n = (Obj.magic n : int) in
  let s = ref ((string_of_int (n))) in
  let i = ref ((String.length (!s) - 3)) in
  (try while (!i > 0) do
    try
  s := ((String.sub (!s) 0 (!i - 0) ^ ",") ^ String.sub (!s) !i (String.length (!s) - !i));
  i := (!i - 3);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!s) : string); raise Return
  with Return -> !__ret)

and main () =
  let __ret = ref (Obj.magic 0) in
  (try
  let data = ref ([[("pm", 10); ("g1", 4); ("s1", 7); ("g2", 6); ("s2", 23); ("d", 16)]; [("pm", 100); ("g1", 14); ("s1", 113); ("g2", 16); ("s2", 1831); ("d", 1718)]; [("pm", 1000); ("g1", 14); ("s1", 113); ("g2", 16); ("s2", 1831); ("d", 1718)]; [("pm", 10000); ("g1", 36); ("s1", 9551); ("g2", 38); ("s2", 30593); ("d", 21042)]; [("pm", 100000); ("g1", 70); ("s1", 173359); ("g2", 72); ("s2", 31397); ("d", 141962)]; [("pm", 1000000); ("g1", 100); ("s1", 396733); ("g2", 102); ("s2", 1444309); ("d", 1047576)]; [("pm", 10000000); ("g1", 148); ("s1", 2010733); ("g2", 150); ("s2", 13626257); ("d", 11615524)]; [("pm", 100000000); ("g1", 198); ("s1", 46006769); ("g2", 200); ("s2", 378043979); ("d", 332037210)]; [("pm", 1000000000); ("g1", 276); ("s1", 649580171); ("g2", 278); ("s2", 4260928601); ("d", 3611348430)]; [("pm", 10000000000); ("g1", 332); ("s1", 5893180121); ("g2", 334); ("s2", 30827138509); ("d", 24933958388)]; [("pm", 100000000000); ("g1", 386); ("s1", 35238645587); ("g2", 388); ("s2", 156798792223); ("d", 121560146636)]]) in
  (try List.iter (fun entry ->
    try
  let pm = commatize (Obj.repr ((try List.assoc ("pm") entry with Not_found -> 0))) in
  let line1 = (("Earliest difference > " ^ pm) ^ " between adjacent prime gap starting primes:") in
  print_endline (__show line1);
  let line2 = (((((((((("Gap " ^ (string_of_int ((try List.assoc ("g1") entry with Not_found -> 0)))) ^ " starts at ") ^ commatize (Obj.repr ((try List.assoc ("s1") entry with Not_found -> 0)))) ^ ", gap ") ^ (string_of_int ((try List.assoc ("g2") entry with Not_found -> 0)))) ^ " starts at ") ^ commatize (Obj.repr ((try List.assoc ("s2") entry with Not_found -> 0)))) ^ ", difference is ") ^ commatize (Obj.repr ((try List.assoc ("d") entry with Not_found -> 0)))) ^ ".") in
  print_endline (__show line2);
  print_endline ("");
    with Continue -> ()) (!data) with Break -> ());
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