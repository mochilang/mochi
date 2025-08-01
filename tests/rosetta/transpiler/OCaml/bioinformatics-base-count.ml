(* Generated by Mochi transpiler v0.10.42 on 2025-07-27 21:41 +0700 *)


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

let rec padLeft s w =
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

let dna = (((((((((("" ^ "CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATG") ^ "CTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTG") ^ "AGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGAT") ^ "GGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT") ^ "CGATTCTGCTTATAACACTATGTTCTTATGAAATGGATGTTCTGAGTTGG") ^ "TCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA") ^ "TTTAATTTTTCTATATAGCGATCTGTATTTAAGCAATTCATTTAGGTTAT") ^ "CGCCGCGATGCTCGGTTCGGACCGCCAAGCATCTGGCTCCACTGCTAGTG") ^ "TCCTAAATTTGAATGGCAAACACAAATAAGATTTAGCAATTCGTGTAGAC") ^ "GACCGGGGACTTGCATGATGGGAGCAGCTTTGTTAAACTACGAACGTAAT")
let le = String.length (dna)
let i = ref (0)
let a = ref (0)
let c = ref (0)
let g = ref (0)
let t = ref (0)
let idx = ref (0)
let () =
  let mem_start = _mem () in
  let start = _now () in
  print_endline ("SEQUENCE:");
  (try while (!i < le) do
    try
  let k = ref ((!i + 50)) in
  if (!k > le) then (
  k := le;
  );
  print_endline (((padLeft ((string_of_int (!i))) (5) ^ ": ") ^ String.sub (dna) !i (!k - !i)));
  i := (!i + 50);
    with Continue -> ()
  done with Break -> ());
  (try while (!idx < le) do
    try
  let ch = String.sub (dna) !idx ((!idx + 1) - !idx) in
  if (ch = "A") then (
  a := (!a + 1);
  ) else (
  if (ch = "C") then (
  c := (!c + 1);
  ) else (
  if (ch = "G") then (
  g := (!g + 1);
  ) else (
  if (ch = "T") then (
  t := (!t + 1);
  );
  );
  );
  );
  idx := (!idx + 1);
    with Continue -> ()
  done with Break -> ());
  print_endline ("");
  print_endline ("BASE COUNT:");
  print_endline (("    A: " ^ padLeft ((string_of_int (!a))) (3)));
  print_endline (("    C: " ^ padLeft ((string_of_int (!c))) (3)));
  print_endline (("    G: " ^ padLeft ((string_of_int (!g))) (3)));
  print_endline (("    T: " ^ padLeft ((string_of_int (!t))) (3)));
  print_endline ("    ------");
  print_endline (("    Σ: " ^ (string_of_int (le))));
  print_endline ("    ======");
  let finish = _now () in
  let mem_end = _mem () in
  let dur = (finish - start) / 1000 in
  let mem_bytes = max 0 (mem_end - mem_start) in
  Printf.printf "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"%s\"\n}\n" dur mem_bytes "main";
