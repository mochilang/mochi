(* Generated by Mochi transpiler v0.10.39 on 2025-07-25 00:13 +0700 *)


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

exception Break
exception Continue

exception Return

let rec pow10 exp =
  let __ret = ref 0 in
  (try
  let n = ref (1) in
  let i = ref (0) in
  (try while (!i < exp) do
    try
  n := (!n * 10);
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := !n; raise Return
  with Return -> !__ret)

let rec totient n =
  let __ret = ref 0 in
  (try
  let tot = ref (n) in
  let nn = ref (n) in
  let i = ref (2) in
  (try while ((!i * !i) <= !nn) do
    try
  if ((!nn mod !i) = 0) then (
  (try while ((!nn mod !i) = 0) do
    try
  nn := (!nn / !i);
    with Continue -> ()
  done with Break -> ());
  tot := (!tot - (!tot / !i));
  );
  if (!i = 2) then (
  i := 1;
  );
  i := (!i + 2);
    with Continue -> ()
  done with Break -> ());
  if (!nn > 1) then (
  tot := (!tot - (!tot / !nn));
  );
  __ret := !tot; raise Return
  with Return -> !__ret)

let pps = ref ([])
let rec getPerfectPowers maxExp =
  let __ret = ref (Obj.magic 0) in
  (try
  let upper = pow10 (maxExp) in
  let i = ref (2) in
  (try while ((!i * !i) < upper) do
    try
  let p = ref (!i) in
  (try while true do
    try
  p := (!p * !i);
  if (!p >= upper) then (
  raise Break;
  );
  pps := ((!p, Obj.repr true) :: List.remove_assoc !p !pps);
    with Continue -> ()
  done with Break -> ());
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
    !__ret
  with Return -> !__ret) in
let rec getAchilles minExp maxExp =
  let __ret = ref [] in
  (try
  let lower = pow10 (minExp) in
  let upper = pow10 (maxExp) in
  let achilles = ref ([]) in
  let b = ref (1) in
  (try while (((!b * !b) * !b) < upper) do
    try
  let b3 = ((!b * !b) * !b) in
  let a = ref (1) in
  (try while true do
    try
  let p = ((b3 * !a) * !a) in
  if (p >= upper) then (
  raise Break;
  );
  if (p >= lower) then (
  if not ((List.mem_assoc p !pps)) then (
  achilles := ((p, Obj.repr true) :: List.remove_assoc p !achilles);
  );
  );
  a := (!a + 1);
    with Continue -> ()
  done with Break -> ());
  b := (!b + 1);
    with Continue -> ()
  done with Break -> ());
  __ret := !achilles; raise Return
  with Return -> !__ret) in
let rec sortInts xs =
  let __ret = ref [] in
  (try
  let res = ref ([]) in
  let tmp = ref (xs) in
  (try while (List.length (!tmp) > 0) do
    try
  let min = ref (List.nth (!tmp) (0)) in
  let idx = ref (0) in
  let i = ref (1) in
  (try while (!i < List.length (!tmp)) do
    try
  if (List.nth (!tmp) (!i) < !min) then (
  min := List.nth (!tmp) (!i);
  idx := !i;
  );
  i := (!i + 1);
    with Continue -> ()
  done with Break -> ());
  res := (!res @ [!min]);
  let out = ref ([]) in
  let j = ref (0) in
  (try while (!j < List.length (!tmp)) do
    try
  if (!j <> !idx) then (
  out := (!out @ [List.nth (!tmp) (!j)]);
  );
  j := (!j + 1);
    with Continue -> ()
  done with Break -> ());
  tmp := !out;
    with Continue -> ()
  done with Break -> ());
  __ret := !res; raise Return
  with Return -> !__ret) in
let rec pad n width =
  let __ret = ref "" in
  (try
  let s = ref ((string_of_int (n))) in
  (try while (String.length (!s) < width) do
    try
  s := (" " ^ !s);
    with Continue -> ()
  done with Break -> ());
  __ret := !s; raise Return
  with Return -> !__ret) in
let rec main () =
  let __ret = ref (Obj.magic 0) in
  (try
  let maxDigits = 15 in
  ignore (getPerfectPowers (5));
  let achSet = ref (getAchilles (1) (5)) in
  let ach = ref ([]) in
  (try List.iter (fun k ->
    try
  ach := (!ach @ [k]);
    with Continue -> ()) (List.map fst !achSet) with Break -> ());
  ach := sortInts (!ach);
  print_endline ("First 50 Achilles numbers:");
  let i = ref (0) in
  (try while (!i < 50) do
    try
  let line = ref ("") in
  let j = ref (0) in
  (try while (!j < 10) do
    try
  line := (!line ^ pad (List.nth (!ach) (!i)) (4));
  if (!j < 9) then (
  line := (!line ^ " ");
  );
  i := (!i + 1);
  j := (!j + 1);
    with Continue -> ()
  done with Break -> ());
  print_endline (__show !line);
    with Continue -> ()
  done with Break -> ());
  print_endline ("\nFirst 30 strong Achilles numbers:");
  let strong = ref ([]) in
  let count = ref (0) in
  let idx = ref (0) in
  (try while (!count < 30) do
    try
  let tot = totient (List.nth (!ach) (!idx)) in
  if (List.mem_assoc tot !achSet) then (
  strong := (!strong @ [List.nth (!ach) (!idx)]);
  count := (!count + 1);
  );
  idx := (!idx + 1);
    with Continue -> ()
  done with Break -> ());
  i := 0;
  (try while (!i < 30) do
    try
  let line = ref ("") in
  let j = ref (0) in
  (try while (!j < 10) do
    try
  line := (!line ^ pad (List.nth (!strong) (!i)) (5));
  if (!j < 9) then (
  line := (!line ^ " ");
  );
  i := (!i + 1);
  j := (!j + 1);
    with Continue -> ()
  done with Break -> ());
  print_endline (__show !line);
    with Continue -> ()
  done with Break -> ());
  print_endline ("\nNumber of Achilles numbers with:");
  let counts = ref ([1; 12; 47; 192; 664; 2242; 7395; 24008; 77330; 247449; 788855; 2508051; 7960336; 25235383]) in
  let d = ref (2) in
  (try while (!d <= maxDigits) do
    try
  let c = List.nth (!counts) ((!d - 2)) in
  print_endline (((pad (!d) (2) ^ " digits: ") ^ (string_of_int (c))));
  d := (!d + 1);
    with Continue -> ()
  done with Break -> ());
    !__ret
  with Return -> !__ret) in
let () =
  ignore (main ());
