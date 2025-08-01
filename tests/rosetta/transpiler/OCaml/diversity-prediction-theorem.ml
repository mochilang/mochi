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

let rec pow10 n =
  let __ret = ref 0.0 in
  (try
  let n = (Obj.magic n : int) in
  let r = ref (1.0) in
  let i = ref (0) in
  (try while (!i < n) do
    try
  r := (!r *. 10.0);
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!r) : float); raise Return
  with Return -> !__ret)

and formatFloat f prec =
  let __ret = ref "" in
  (try
  let f = (Obj.magic f : float) in
  let prec = (Obj.magic prec : int) in
  let scale = pow10 (Obj.repr (prec)) in
  let scaled = ((f *. scale) +. 0.5) in
  let n = ref ((Obj.magic scaled : int)) in
  let digits = ref ((string_of_int (!n))) in
  (try while (String.length (!digits) <= prec) do
    try
  digits := ("0" ^ !digits);
    with Continue -> ()
  done with Break -> ());
  let intPart = String.sub (!digits) 0 ((String.length (!digits) - prec) - 0) in
  let fracPart = String.sub (!digits) (String.length (!digits) - prec) (String.length (!digits) - (String.length (!digits) - prec)) in
  __ret := (Obj.magic (((intPart ^ ".") ^ fracPart)) : string); raise Return
  with Return -> !__ret)

and padLeft s w =
  let __ret = ref "" in
  (try
  let w = (Obj.magic w : int) in
  let res = ref ("") in
  let n = ref ((w - String.length (s))) in
  (try while (!n > 0) do
    try
  res := (!res ^ " ");
  n := (!n - 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic ((!res ^ s)) : string); raise Return
  with Return -> !__ret)

and averageSquareDiff f preds =
  let __ret = ref 0.0 in
  (try
  let f = (Obj.magic f : float) in
  let av = ref (0.0) in
  let i = ref (0) in
  (try while (!i < List.length (preds)) do
    try
  av := (!av +. ((List.nth (preds) (!i) -. f) *. (List.nth (preds) (!i) -. f)));
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  av := (!av /. float_of_int (List.length (preds)));
  __ret := (Obj.magic (!av) : float); raise Return
  with Return -> !__ret)

and diversityTheorem truth preds =
  let __ret = ref ([] : float list) in
  (try
  let truth = (Obj.magic truth : float) in
  let av = ref (0.0) in
  let i = ref (0) in
  (try while (!i < List.length (preds)) do
    try
  av := (!av +. List.nth (preds) (!i));
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  av := (!av /. float_of_int (List.length (preds)));
  let avErr = averageSquareDiff (Obj.repr (truth)) (preds) in
  let crowdErr = ((truth -. !av) *. (truth -. !av)) in
  let div = averageSquareDiff (Obj.repr (!av)) (preds) in
  __ret := (Obj.magic ([avErr; crowdErr; div]) : float list); raise Return
  with Return -> !__ret)

and main () =
  let __ret = ref (Obj.magic 0) in
  (try
  let predsArray = ref ([[48.0; 47.0; 51.0]; [48.0; 47.0; 51.0; 42.0]]) in
  let truth = 49.0 in
  let i = ref (0) in
  (try while (!i < List.length (!predsArray)) do
    try
  let preds = ref (List.nth (!predsArray) (!i)) in
  let res = ref (diversityTheorem (Obj.repr (truth)) (!preds)) in
  print_endline (("Average-error : " ^ padLeft (formatFloat (Obj.repr (List.nth (!res) (0))) (Obj.repr (3))) (Obj.repr (6))));
  print_endline (("Crowd-error   : " ^ padLeft (formatFloat (Obj.repr (List.nth (!res) (1))) (Obj.repr (3))) (Obj.repr (6))));
  print_endline (("Diversity     : " ^ padLeft (formatFloat (Obj.repr (List.nth (!res) (2))) (Obj.repr (3))) (Obj.repr (6))));
  print_endline ("");
  i := (!i + 1);
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
  ()