indirect enum Tree {
    case leaf
    case node(left: Tree, value: Int, right: Tree)
}

func sumTree(_ t: Tree) -> Int {
    switch t {
    case .leaf: return 0
    case let .node(left, value, right):
        return sumTree(left) + value + sumTree(right)
    }
}
let t = Tree.node(left: .leaf, value: 1, right: .node(left: .leaf, value: 2, right: .leaf))
print(sumTree(t))
