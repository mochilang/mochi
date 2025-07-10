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


type person = { mutable name : string; mutable age : int; mutable status : string }
let people : person list = [{ name = "Alice"; age = 17; status = "minor" };{ name = "Bob"; age = 25; status = "unknown" };{ name = "Charlie"; age = 18; status = "unknown" };{ name = "Diana"; age = 16; status = "minor" }]

let () =
  let people =
    List.map (fun __it ->
      if (age >= 18) then { __it with status = "adult"; age = (age + 1) } else __it
    ) __it
  ) people
assert ((people = [{ name = "Alice"; age = 17; status = "minor" };{ name = "Bob"; age = 26; status = "adult" };{ name = "Charlie"; age = 19; status = "adult" };{ name = "Diana"; age = 16; status = "minor" }]))
print_endline (__show ("ok"));
