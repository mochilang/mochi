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


type tree = Leaf | Node of tree * int * tree
let rec sum_tree (t : tree) : int =
  (match t with | Leaf -> 0 | Node (left, value, right) -> ((sum_tree left + value) + sum_tree right))

let t = Node (Leaf, 1, Node (Leaf, 2, Leaf))

let () =
  print_endline (__show (sum_tree t));
