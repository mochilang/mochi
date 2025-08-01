(* Generated by Mochi transpiler v0.10.42 on 2025-07-27 17:37 UTC *)


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

exception Break
exception Continue

exception Return

let rec indexOf s ch =
  let __ret = ref 0 in
  (try
  let i = ref (0) in
  (try while (!i < String.length (s)) do
    try
  if (String.sub (s) !i ((!i + 1) - !i) = ch) then (
  __ret := (Obj.magic (!i) : int); raise Return
  );
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (-(1)) : int); raise Return
  with Return -> !__ret)

let rec fields s =
  let __ret = ref ([] : string list) in
  (try
  let words = ref ([]) in
  let cur = ref ("") in
  let i = ref (0) in
  (try while (!i < String.length (s)) do
    try
  let ch = String.sub (s) !i ((!i + 1) - !i) in
  if (((ch = " ") || (ch = "\t")) || (ch = "\n")) then (
  if (String.length (!cur) > 0) then (
  words := (List.append !words [!cur]);
  cur := "";
  );
  ) else (
  cur := (!cur ^ ch);
  );
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  if (String.length (!cur) > 0) then (
  words := (List.append !words [!cur]);
  );
  __ret := (Obj.magic (!words) : string list); raise Return
  with Return -> !__ret)

let rec makePatterns () =
  let __ret = ref ([] : string list) in
  (try
  let digits = ref (["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"]) in
  let pats = ref ([]) in
  let i = ref (0) in
  (try while (!i < List.length (!digits)) do
    try
  let j = ref (0) in
  (try while (!j < List.length (!digits)) do
    try
  if (!j <> !i) then (
  let k = ref (0) in
  (try while (!k < List.length (!digits)) do
    try
  if ((!k <> !i) && (!k <> !j)) then (
  let l = ref (0) in
  (try while (!l < List.length (!digits)) do
    try
  if (((!l <> !i) && (!l <> !j)) && (!l <> !k)) then (
  pats := (List.append !pats [(((List.nth (!digits) (!i) ^ List.nth (!digits) (!j)) ^ List.nth (!digits) (!k)) ^ List.nth (!digits) (!l))]);
  );
  l := (!l + 1);
    with Continue -> ()
  done with Break -> ());
  );
  k := (!k + 1);
    with Continue -> ()
  done with Break -> ());
  );
  j := (!j + 1);
    with Continue -> ()
  done with Break -> ());
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!pats) : string list); raise Return
  with Return -> !__ret)

let rec main () =
  let __ret = ref (Obj.magic 0) in
  (try
  print_endline (((((("Cows and bulls/player\n" ^ "You think of four digit number of unique digits in the range 1 to 9.\n") ^ "I guess.  You score my guess:\n") ^ "    A correct digit but not in the correct place is a cow.\n") ^ "    A correct digit in the correct place is a bull.\n") ^ "You give my score as two numbers separated with a space."));
  let patterns = ref (makePatterns ()) in
  (try while true do
    try
  if (List.length (!patterns) = 0) then (
  print_endline ("Oops, check scoring.");
  __ret := (); raise Return
  );
  let guess = List.nth (!patterns) (0) in
  patterns := List.of_seq (Seq.take (List.length (!patterns) - 1) (Seq.drop 1 (List.to_seq !patterns)));
  let cows = ref (0) in
  let bulls = ref (0) in
  (try while true do
    try
  print_endline ((("My guess: " ^ guess) ^ ".  Score? (c b) "));
  let line = (try read_line () with End_of_file -> "") in
  let toks = ref (fields (line)) in
  if (List.length (!toks) = 2) then (
  let c = int_of_string (List.nth (!toks) (0)) in
  let b = int_of_string (List.nth (!toks) (1)) in
  if (((((c >= 0) && (c <= 4)) && (b >= 0)) && (b <= 4)) && ((c + b) <= 4)) then (
  cows := c;
  bulls := b;
  raise Break;
  );
  );
  print_endline ("Score guess as two numbers: cows bulls");
    with Continue -> ()
  done with Break -> ());
  if (!bulls = 4) then (
  print_endline ("I did it. :)");
  __ret := (); raise Return
  );
  let next = ref ([]) in
  let idx = ref (0) in
  (try while (!idx < List.length (!patterns)) do
    try
  let pat = List.nth (!patterns) (!idx) in
  let c = ref (0) in
  let b = ref (0) in
  let i = ref (0) in
  (try while (!i < 4) do
    try
  let cg = String.sub (guess) !i ((!i + 1) - !i) in
  let cp = String.sub (pat) !i ((!i + 1) - !i) in
  if (cg = cp) then (
  b := (!b + 1);
  ) else (
  if ((try String.index (pat) (String.get (cg) 0) with Not_found -> -1) >= 0) then (
  c := (!c + 1);
  );
  );
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  if ((!c = !cows) && (!b = !bulls)) then (
  next := (List.append !next [pat]);
  );
  idx := (!idx + 1);
    with Continue -> ()
  done with Break -> ());
  patterns := !next;
    with Continue -> ()
  done with Break -> ());
    !__ret
  with Return -> !__ret)

let () =
  ignore (main ());
