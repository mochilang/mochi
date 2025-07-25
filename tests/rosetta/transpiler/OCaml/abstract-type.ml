(* Generated by Mochi transpiler v0.10.39 on 2025-07-24 20:13 +0700 *)


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

exception Return

let rec beastKind b =
  let __ret = ref "" in
  (try
  __ret := (match b with | [("tag", "Dog"); ("kind", k); ("name", _)] -> k | [("tag", "Cat"); ("kind", k); ("name", _)] -> k); raise Return;
  __ret := (match b with | [("tag", "Dog"); ("kind", k); ("name", _)] -> k | [("tag", "Cat"); ("kind", k); ("name", _)] -> k);
    !__ret
  with Return -> !__ret)

let rec beastName b =
  let __ret = ref "" in
  (try
  __ret := (match b with | [("tag", "Dog"); ("kind", _); ("name", n)] -> n | [("tag", "Cat"); ("kind", _); ("name", n)] -> n); raise Return;
  __ret := (match b with | [("tag", "Dog"); ("kind", _); ("name", n)] -> n | [("tag", "Cat"); ("kind", _); ("name", n)] -> n);
    !__ret
  with Return -> !__ret)

let rec beastCry b =
  let __ret = ref "" in
  (try
  __ret := (match b with | [("tag", "Dog"); ("kind", _); ("name", _)] -> "Woof" | [("tag", "Cat"); ("kind", _); ("name", _)] -> "Meow"); raise Return;
  __ret := (match b with | [("tag", "Dog"); ("kind", _); ("name", _)] -> "Woof" | [("tag", "Cat"); ("kind", _); ("name", _)] -> "Meow");
    !__ret
  with Return -> !__ret)

let rec bprint b =
  let __ret = ref () in
  (try
  print_endline ((((((beastName (b) ^ ", who's a ") ^ beastKind (b)) ^ ", cries: \"") ^ beastCry (b)) ^ "\"."));
    !__ret
  with Return -> !__ret)

let rec main () =
  let __ret = ref () in
  (try
  let d = ref ([("tag", "Dog"); ("kind", "labrador"); ("name", "Max")]) in
  let c = ref ([("tag", "Cat"); ("kind", "siamese"); ("name", "Sammy")]) in
  ignore (bprint (!d));
  ignore (bprint (!c));
    !__ret
  with Return -> !__ret)

let () =
  ignore (main ());
