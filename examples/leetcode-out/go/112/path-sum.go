package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

type Tree interface { isTree() }
type Leaf struct {
}
func (Leaf) isTree() {}
type Node struct {
	Left Tree `json:"left"`
	Value int `json:"value"`
	Right Tree `json:"right"`
}
func (Node) isTree() {}

func isLeaf(t Tree) bool {
	return func() bool {
	_t := t
	if _, ok := _t.(Leaf); ok {
		return true
	}
	return false
}()
}

func hasPathSum(root Tree, target int) bool {
	var nodeSum = func(l Tree, v int, r Tree, remaining int) bool {
		var leftLeaf bool = isLeaf(l)
		var rightLeaf bool = isLeaf(r)
		if (leftLeaf && rightLeaf) {
			return (remaining == 0)
		}
		return (hasPathSum(l, remaining) || hasPathSum(r, remaining))
}
	return func() bool {
	_t := root
	if _, ok := _t.(Leaf); ok {
		return false
	}
	if _tmp0, ok := _t.(Node); ok {
		l := _tmp0.Left
		v := _tmp0.Value
		r := _tmp0.Right
		return nodeSum(l, v, r, (target - v))
	}
	var _zero bool
	return _zero
}()
}

func example_1() {
	var root Node = Node{Left: Node{Left: Node{Left: Leaf{}, Value: 7, Right: Leaf{}}, Value: 11, Right: Node{Left: Leaf{}, Value: 2, Right: Leaf{}}}, Value: 4, Right: Leaf{}}
	var tree Node = Node{Left: root, Value: 5, Right: Node{Left: Node{Left: Leaf{}, Value: 13, Right: Leaf{}}, Value: 8, Right: Node{Left: Leaf{}, Value: 4, Right: Node{Left: Leaf{}, Value: 1, Right: Leaf{}}}}}
	_ = tree
	expect((hasPathSum(tree, 22) == true))
}

func example_2() {
	var tree Node = Node{Left: Node{Left: Leaf{}, Value: 2, Right: Leaf{}}, Value: 1, Right: Node{Left: Leaf{}, Value: 3, Right: Leaf{}}}
	_ = tree
	expect((hasPathSum(tree, 5) == false))
}

func example_3() {
	var tree Node = Node{Left: Node{Left: Leaf{}, Value: 2, Right: Leaf{}}, Value: 1, Right: Leaf{}}
	_ = tree
	expect((hasPathSum(tree, 0) == false))
}

func single_node() {
	expect((hasPathSum(Node{Left: Leaf{}, Value: 5, Right: Leaf{}}, 5) == true))
}

func empty() {
	expect((hasPathSum(Leaf{}, 1) == false))
}

func main() {
	example_1()
	example_2()
	example_3()
	single_node()
	empty()
}

