 type tree = Leaf | Node of tree * int * tree
 let rec sum_tree t =
   match t with
   | Leaf -> 0
   | Node (left, value, right) -> sum_tree left + value + sum_tree right
 let t = Node (Leaf, 1, Node (Leaf, 2, Leaf))
 let () = print_endline (string_of_int (sum_tree t))
