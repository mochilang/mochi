type Tree =
    | Leaf
    | Node of Tree * int * Tree

let rec sumTree t =
    match t with
    | Leaf -> 0
    | Node(l,v,r) -> sumTree l + v + sumTree r

let t = Node(Leaf, 1, Node(Leaf, 2, Leaf))
printfn "%d" (sumTree t)
