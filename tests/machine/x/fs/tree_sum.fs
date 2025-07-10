open System

type Tree =
    | Leaf
    | Node of Tree * int * Tree
let sum_tree (t) =
    (match t with
    | Leaf -> 0
    | Node left value right -> sum_tree left + value + sum_tree right)
let t = Node(Leaf, 1, Node(Leaf, 2, Leaf))
printfn "%A" (sum_tree t)
