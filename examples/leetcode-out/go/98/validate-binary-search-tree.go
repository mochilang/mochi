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

type MaybeInt interface { isMaybeInt() }
type None struct {
}
func (None) isMaybeInt() {}
type Some struct {
	Value int `json:"value"`
}
func (Some) isMaybeInt() {}

func helper(node Tree, low MaybeInt, high MaybeInt) bool {
	return func() bool {
	_t := node
	if _, ok := _t.(Leaf); ok {
		return true
	}
	if _tmp2, ok := _t.(Node); ok {
		l := _tmp2.Left
		v := _tmp2.Value
		r := _tmp2.Right
		return ((((func() bool {
	_t := low
	if _tmp0, ok := _t.(Some); ok {
		x := _tmp0.Value
		return (v > x)
	}
	if _, ok := _t.(None); ok {
		return true
	}
	var _zero bool
	return _zero
}()) && (func() bool {
	_t := high
	if _tmp1, ok := _t.(Some); ok {
		y := _tmp1.Value
		return (v < y)
	}
	if _, ok := _t.(None); ok {
		return true
	}
	var _zero bool
	return _zero
}())) && helper(l, low, Some{Value: v})) && helper(r, Some{Value: v}, high))
	}
	var _zero bool
	return _zero
}()
}

func isValidBST(root Tree) bool {
	return helper(root, None{}, None{})
}

func example_1() {
	var tree Node = Node{Left: Node{Left: Leaf{}, Value: 1, Right: Leaf{}}, Value: 2, Right: Node{Left: Leaf{}, Value: 3, Right: Leaf{}}}
	_ = tree
	expect((isValidBST(tree) == true))
}

func example_2() {
	var tree Node = Node{Left: Node{Left: Leaf{}, Value: 1, Right: Leaf{}}, Value: 5, Right: Node{Left: Node{Left: Leaf{}, Value: 3, Right: Leaf{}}, Value: 4, Right: Node{Left: Leaf{}, Value: 6, Right: Leaf{}}}}
	_ = tree
	expect((isValidBST(tree) == false))
}

func main() {
	example_1()
	example_2()
}

