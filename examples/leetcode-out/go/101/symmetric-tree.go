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

func isSymmetric(root Tree) bool {
	var isMirror func(Tree, Tree) bool
	isMirror = func(t1 Tree, t2 Tree) bool {
		return func() bool {
		_t := t1
		if _, ok := _t.(Leaf); ok {
			return func() bool {
		_t := t2
		if _, ok := _t.(Leaf); ok {
			return true
		}
		return false
	}()
		}
		if _tmp1, ok := _t.(Node); ok {
			l1 := _tmp1.Left
			v1 := _tmp1.Value
			r1 := _tmp1.Right
			return func() bool {
		_t := t2
		if _tmp0, ok := _t.(Node); ok {
			l2 := _tmp0.Left
			v2 := _tmp0.Value
			r2 := _tmp0.Right
			return (((v1 == v2) && isMirror(l1, r2)) && isMirror(r1, l2))
		}
		return false
	}()
		}
		var _zero bool
		return _zero
	}()
}
	return isMirror(root, root)
}

func example_1() {
	var tree Node = Node{Left: Node{Left: Node{Left: Leaf{}, Value: 3, Right: Leaf{}}, Value: 2, Right: Node{Left: Leaf{}, Value: 4, Right: Leaf{}}}, Value: 1, Right: Node{Left: Node{Left: Leaf{}, Value: 4, Right: Leaf{}}, Value: 2, Right: Node{Left: Leaf{}, Value: 3, Right: Leaf{}}}}
	_ = tree
	expect((isSymmetric(tree) == true))
}

func example_2() {
	var tree Node = Node{Left: Node{Left: Leaf{}, Value: 2, Right: Node{Left: Leaf{}, Value: 3, Right: Leaf{}}}, Value: 1, Right: Node{Left: Leaf{}, Value: 2, Right: Node{Left: Leaf{}, Value: 3, Right: Leaf{}}}}
	_ = tree
	expect((isSymmetric(tree) == false))
}

func single_node() {
	expect((isSymmetric(Node{Left: Leaf{}, Value: 1, Right: Leaf{}}) == true))
}

func empty() {
	expect((isSymmetric(Leaf{}) == true))
}

func main() {
	example_1()
	example_2()
	single_node()
	empty()
}

