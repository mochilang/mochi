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

func minDepth(root Tree) int {
	var helper = func(left Tree, right Tree) int {
		var leftDepth int = minDepth(left)
		var rightDepth int = minDepth(right)
		if ((leftDepth == 0) && (rightDepth == 0)) {
			return 1
		}
		if (leftDepth == 0) {
			return (rightDepth + 1)
		}
		if (rightDepth == 0) {
			return (leftDepth + 1)
		}
		if (leftDepth < rightDepth) {
			return (leftDepth + 1)
		}
		return (rightDepth + 1)
}
	return func() int {
	_t := root
	if _, ok := _t.(Leaf); ok {
		return 0
	}
	if _tmp0, ok := _t.(Node); ok {
		l := _tmp0.Left
		r := _tmp0.Right
		return helper(l, r)
	}
	var _zero int
	return _zero
}()
}

func example_1() {
	var tree Node = Node{Left: Node{Left: Leaf{}, Value: 9, Right: Leaf{}}, Value: 3, Right: Node{Left: Node{Left: Leaf{}, Value: 15, Right: Leaf{}}, Value: 20, Right: Node{Left: Leaf{}, Value: 7, Right: Leaf{}}}}
	_ = tree
	expect((minDepth(tree) == 2))
}

func example_2() {
	var tree Node = Node{Left: Leaf{}, Value: 2, Right: Node{Left: Leaf{}, Value: 3, Right: Leaf{}}}
	_ = tree
	expect((minDepth(tree) == 2))
}

func single_node() {
	expect((minDepth(Node{Left: Leaf{}, Value: 1, Right: Leaf{}}) == 1))
}

func empty() {
	expect((minDepth(Leaf{}) == 0))
}

func main() {
	example_1()
	example_2()
	single_node()
	empty()
}

