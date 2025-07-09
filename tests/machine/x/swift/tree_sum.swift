indirect enum Tree {
    case leaf
    case node(left: Tree, value: Int, right: Tree)
}
func sum_tree(_ t: Tree) -> Int {
    return { () in
    switch t {
    case Tree.leaf: return 0
    case let .node(left, value, right): return sum_tree(left) + value + sum_tree(right)
    }
}()
}
let t = Tree.node(left: Tree.leaf, value: 1, right: Tree.node(left: Tree.leaf, value: 2, right: Tree.leaf))
print(sum_tree(t))
