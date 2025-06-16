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

func countNodes(root Tree) int {
	return func() int {
	_t := root
	if _, ok := _t.(Leaf); ok {
		return 0
	}
	if _tmp0, ok := _t.(Node); ok {
		l := _tmp0.Left
		r := _tmp0.Right
		return ((countNodes(l) + countNodes(r)) + 1)
	}
	var _zero int
	return _zero
}()
}

func example_1() {
	expect((countNodes(example1) == 6))
}

func single_node() {
	expect((countNodes(Node{Left: Leaf{}, Value: 1, Right: Leaf{}}) == 1))
}

func empty() {
	expect((countNodes(Leaf{}) == 0))
}

var example1 Node = Node{Left: Node{Left: Node{Left: Leaf{}, Value: 4, Right: Leaf{}}, Value: 2, Right: Node{Left: Leaf{}, Value: 5, Right: Leaf{}}}, Value: 1, Right: Node{Left: Node{Left: Leaf{}, Value: 6, Right: Leaf{}}, Value: 3, Right: Leaf{}}}
func main() {
	example_1()
	single_node()
	empty()
}

