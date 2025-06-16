package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

type Node struct {
	Value int `json:"value"`
	Left int `json:"left"`
	Right int `json:"right"`
}

func max(a int, b int) int {
	if (a > b) {
		return a
	} else {
		return b
	}
}

func abs(x int) int {
	if (x < 0) {
		return -x
	} else {
		return x
	}
}

func height(tree []Node, idx int) int {
	if (idx == (-1)) {
		return 0
	}
	var node Node = tree[idx]
	_ = node
	return (max(height(tree, node.Left), height(tree, node.Right)) + 1)
}

func balanced(tree []Node, idx int) bool {
	if (idx == (-1)) {
		return true
	}
	var node Node = tree[idx]
	_ = node
	var lh int = height(tree, node.Left)
	var rh int = height(tree, node.Right)
	return ((balanced(tree, node.Left) && balanced(tree, node.Right)) && (abs((lh - rh)) <= 1))
}

func isBalanced(tree []Node, root int) bool {
	return balanced(tree, root)
}

func example_1() {
	var tree []Node = []Node{Node{Value: 3, Left: 1, Right: 2}, Node{Value: 9, Left: -1, Right: -1}, Node{Value: 20, Left: 3, Right: 4}, Node{Value: 15, Left: -1, Right: -1}, Node{Value: 7, Left: -1, Right: -1}}
	_ = tree
	expect((isBalanced(tree, 0) == true))
}

func example_2() {
	var tree []Node = []Node{Node{Value: 1, Left: 1, Right: 2}, Node{Value: 2, Left: 3, Right: 4}, Node{Value: 2, Left: -1, Right: -1}, Node{Value: 3, Left: 5, Right: 6}, Node{Value: 3, Left: -1, Right: -1}, Node{Value: 4, Left: -1, Right: -1}, Node{Value: 4, Left: -1, Right: -1}}
	_ = tree
	expect((isBalanced(tree, 0) == false))
}

func single_node() {
	var tree []Node = []Node{Node{Value: 1, Left: -1, Right: -1}}
	_ = tree
	expect((isBalanced(tree, 0) == true))
}

func empty() {
	var empty []Node = []Node{}
	_ = empty
	expect((isBalanced(empty, -1) == true))
}

func main() {
	example_1()
	example_2()
	single_node()
	empty()
}

