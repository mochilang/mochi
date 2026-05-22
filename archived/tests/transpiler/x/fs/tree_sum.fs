// Generated 2025-07-21 21:44 +0700

type Tree =
    | Leaf
    | Node of obj * int * obj
let rec sum_tree t =
    match t with
| Leaf -> 0
| Node left value right -> ((sum_tree left) + value) + (sum_tree right)
let t: Tree = Node(Leaf, 1, Node(Leaf, 2, Leaf))
printfn "%s" (string (sum_tree t))
