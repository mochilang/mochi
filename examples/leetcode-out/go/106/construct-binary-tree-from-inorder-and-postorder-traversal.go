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

func buildTree(inorder []int, postorder []int) Tree {
	if (len(inorder) == 0) {
		return Leaf{}
	}
	var rootVal int = postorder[(len(postorder) - 1)]
	var idx int = 0
	for ((idx < len(inorder)) && (inorder[idx] != rootVal)) {
		idx = (idx + 1)
	}
	var leftIn []int = inorder[0:idx]
	var rightIn []int = inorder[(idx + 1):len(inorder)]
	var leftPost []int = postorder[0:idx]
	var rightPost []int = postorder[idx:(len(postorder) - 1)]
	return Node{Left: buildTree(leftIn, leftPost), Value: rootVal, Right: buildTree(rightIn, rightPost)}
}

func isSameTree(a Tree, b Tree) bool {
	return func() bool {
	_t := a
	if _, ok := _t.(Leaf); ok {
		return func() bool {
	_t := b
	if _, ok := _t.(Leaf); ok {
		return true
	}
	return false
}()
	}
	if _tmp1, ok := _t.(Node); ok {
		al := _tmp1.Left
		av := _tmp1.Value
		ar := _tmp1.Right
		return func() bool {
	_t := b
	if _tmp0, ok := _t.(Node); ok {
		bl := _tmp0.Left
		bv := _tmp0.Value
		br := _tmp0.Right
		return (((av == bv) && isSameTree(al, bl)) && isSameTree(ar, br))
	}
	return false
}()
	}
	var _zero bool
	return _zero
}()
}

func example_1() {
	var inorder []int = []int{9, 3, 15, 20, 7}
	_ = inorder
	var postorder []int = []int{9, 15, 7, 20, 3}
	_ = postorder
	var expected Node = Node{Left: Node{Left: Leaf{}, Value: 9, Right: Leaf{}}, Value: 3, Right: Node{Left: Node{Left: Leaf{}, Value: 15, Right: Leaf{}}, Value: 20, Right: Node{Left: Leaf{}, Value: 7, Right: Leaf{}}}}
	_ = expected
	expect((isSameTree(buildTree(inorder, postorder), expected) == true))
}

func example_2() {
	var inorder []int = []int{-1}
	_ = inorder
	var postorder []int = []int{-1}
	_ = postorder
	var expected Node = Node{Left: Leaf{}, Value: -1, Right: Leaf{}}
	_ = expected
	expect((isSameTree(buildTree(inorder, postorder), expected) == true))
}

func empty() {
	var result Tree = buildTree([]int{}, []int{})
	_ = result
	expect(func() bool {
	_t := result
	if _, ok := _t.(Leaf); ok {
		return true
	}
	return false
}())
}

func main() {
	example_1()
	example_2()
	empty()
}

