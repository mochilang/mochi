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

func isSameTree(p Tree, q Tree) bool {
	return func() bool {
	_t := p
	if _, ok := _t.(Leaf); ok {
		return func() bool {
	_t := q
	if _, ok := _t.(Leaf); ok {
		return true
	}
	return false
}()
	}
	if _tmp1, ok := _t.(Node); ok {
		pl := _tmp1.Left
		pv := _tmp1.Value
		pr := _tmp1.Right
		return func() bool {
	_t := q
	if _tmp0, ok := _t.(Node); ok {
		ql := _tmp0.Left
		qv := _tmp0.Value
		qr := _tmp0.Right
		return (((pv == qv) && isSameTree(pl, ql)) && isSameTree(pr, qr))
	}
	return false
}()
	}
	var _zero bool
	return _zero
}()
}

func example_1() {
	var p Node = Node{Left: Node{Left: Leaf{}, Value: 2, Right: Leaf{}}, Value: 1, Right: Node{Left: Leaf{}, Value: 3, Right: Leaf{}}}
	_ = p
	var q Node = Node{Left: Node{Left: Leaf{}, Value: 2, Right: Leaf{}}, Value: 1, Right: Node{Left: Leaf{}, Value: 3, Right: Leaf{}}}
	_ = q
	expect((isSameTree(p, q) == true))
}

func example_2() {
	var p Node = Node{Left: Node{Left: Leaf{}, Value: 2, Right: Leaf{}}, Value: 1, Right: Leaf{}}
	_ = p
	var q Node = Node{Left: Leaf{}, Value: 1, Right: Node{Left: Leaf{}, Value: 2, Right: Leaf{}}}
	_ = q
	expect((isSameTree(p, q) == false))
}

func example_3() {
	var p Node = Node{Left: Node{Left: Leaf{}, Value: 2, Right: Leaf{}}, Value: 1, Right: Node{Left: Leaf{}, Value: 1, Right: Leaf{}}}
	_ = p
	var q Node = Node{Left: Node{Left: Leaf{}, Value: 1, Right: Leaf{}}, Value: 1, Right: Node{Left: Leaf{}, Value: 2, Right: Leaf{}}}
	_ = q
	expect((isSameTree(p, q) == false))
}

func both_empty() {
	expect((isSameTree(Leaf{}, Leaf{}) == true))
}

func main() {
	example_1()
	example_2()
	example_3()
	both_empty()
}

