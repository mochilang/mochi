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


type record1 = { mutable a : int }

let xs : int list = [1;2;3]
let ys : int list = (let __res0 = ref [] in
  List.iter (fun x ->
      if ((x mod 2) = 1) then
    __res0 := x :: !__res0;
  ) xs;
List.rev !__res0)

let m : (string * Obj.t) list = { a = 1 }
let s : string = "hello"

let () =
  print_endline (__show ((List.mem 1 ys)));
  print_endline (__show ((List.mem 2 ys)));
  print_endline (List.mem_assoc "a" m);
  print_endline (List.mem_assoc "b" m);
  print_endline (List.mem "ell" s);
  print_endline (List.mem "foo" s);
