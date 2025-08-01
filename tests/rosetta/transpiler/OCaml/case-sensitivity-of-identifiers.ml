(* Generated by Mochi transpiler v0.10.52 on 2025-08-01 21:30 +0700 *)


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

exception Return

let rec main () =
  let __ret = ref (Obj.magic 0) in
  (try
  let pkg_dog = ref ("Salt") in
  let _dog = ref ("Pepper") in
  let pkg_DOG = ref ("Mustard") in
let rec packageSees d1 d2 d3 =
  let __ret = ref ([] : (string * bool) list) in
  (try
  print_endline (((((("Package sees: " ^ d1) ^ " ") ^ d2) ^ " ") ^ d3));
  __ret := (Obj.magic ([("pkg_dog", true); ("Dog", true); ("pkg_DOG", true)]) : (string * bool) list); raise Return
  with Return -> !__ret) in
  let d = ref (packageSees (!pkg_dog) (!_dog) (!pkg_DOG)) in
  print_endline ((("There are " ^ (string_of_int (List.length (!d)))) ^ " dogs.\n"));
  let dog = ref ("Benjamin") in
  d := packageSees (!pkg_dog) (!_dog) (!pkg_DOG);
  print_endline (((((("Main sees:   " ^ !dog) ^ " ") ^ !_dog) ^ " ") ^ !pkg_DOG));
  d := (("dog", true) :: List.remove_assoc "dog" !d);
  d := (("Dog", true) :: List.remove_assoc "Dog" !d);
  d := (("pkg_DOG", true) :: List.remove_assoc "pkg_DOG" !d);
  print_endline ((("There are " ^ (string_of_int (List.length (!d)))) ^ " dogs.\n"));
  _dog := "Samba";
  d := packageSees (!pkg_dog) (!_dog) (!pkg_DOG);
  print_endline (((((("Main sees:   " ^ !dog) ^ " ") ^ !_dog) ^ " ") ^ !pkg_DOG));
  d := (("dog", true) :: List.remove_assoc "dog" !d);
  d := (("Dog", true) :: List.remove_assoc "Dog" !d);
  d := (("pkg_DOG", true) :: List.remove_assoc "pkg_DOG" !d);
  print_endline ((("There are " ^ (string_of_int (List.length (!d)))) ^ " dogs.\n"));
  let _dog = ref ("Bernie") in
  d := packageSees (!pkg_dog) (!_dog) (!pkg_DOG);
  print_endline (((((("Main sees:   " ^ !dog) ^ " ") ^ !_dog) ^ " ") ^ !_dog));
  d := (("dog", true) :: List.remove_assoc "dog" !d);
  d := (("Dog", true) :: List.remove_assoc "Dog" !d);
  d := (("pkg_DOG", true) :: List.remove_assoc "pkg_DOG" !d);
  d := (("DOG", true) :: List.remove_assoc "DOG" !d);
  print_endline ((("There are " ^ (string_of_int (List.length (!d)))) ^ " dogs."));
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