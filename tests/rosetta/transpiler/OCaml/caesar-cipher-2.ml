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

let rec ord ch =
  let __ret = ref 0 in
  (try
  let upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
  let lower = "abcdefghijklmnopqrstuvwxyz" in
  let idx = ref ((try String.index (upper) (String.get (ch) 0) with Not_found -> -1)) in
  if (!idx >= 0) then (
  __ret := (Obj.magic ((65 + !idx)) : int); raise Return
  );
  idx := (try String.index (lower) (String.get (ch) 0) with Not_found -> -1);
  if (!idx >= 0) then (
  __ret := (Obj.magic ((97 + !idx)) : int); raise Return
  );
  __ret := (Obj.magic (0) : int); raise Return
  with Return -> !__ret)

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
  __ret := (Obj.magic ("?") : string); raise Return
  with Return -> !__ret)

let rec shiftRune r k =
  let __ret = ref "" in
  (try
  let k = (Obj.magic k : int) in
  if ((r >= "a") && (r <= "z")) then (
  __ret := (Obj.magic (chr (((((ord (r) - 97) + k) mod 26) + 97))) : string); raise Return
  );
  if ((r >= "A") && (r <= "Z")) then (
  __ret := (Obj.magic (chr (((((ord (r) - 65) + k) mod 26) + 65))) : string); raise Return
  );
  __ret := (Obj.magic (r) : string); raise Return
  with Return -> !__ret)

let rec encipher s k =
  let __ret = ref "" in
  (try
  let k = (Obj.magic k : int) in
  let out = ref ("") in
  let i = ref (0) in
  (try while (!i < String.length (s)) do
    try
  out := (!out ^ shiftRune (String.sub (s) !i ((!i + 1) - !i)) (k));
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := (Obj.magic (!out) : string); raise Return
  with Return -> !__ret)

let rec decipher s k =
  let __ret = ref "" in
  (try
  let k = (Obj.magic k : int) in
  __ret := (Obj.magic (encipher (s) (((26 - (k mod 26)) mod 26))) : string); raise Return
  with Return -> !__ret)

let rec main () =
  let __ret = ref (Obj.magic 0) in
  (try
  let pt = "The five boxing wizards jump quickly" in
  print_endline (("Plaintext: " ^ pt));
  (try List.iter (fun key ->
    try
  if ((key < 1) || (key > 25)) then (
  print_endline ((("Key " ^ (string_of_int (key))) ^ " invalid"));
  raise Continue;
  );
  let ct = encipher (pt) (key) in
  print_endline (("Key " ^ (string_of_int (key))));
  print_endline (("  Enciphered: " ^ ct));
  print_endline (("  Deciphered: " ^ decipher (ct) (key)));
    with Continue -> ()) [0; 1; 7; 25; 26] with Break -> ());
    !__ret
  with Return -> !__ret)

let () =
  ignore (main ());
