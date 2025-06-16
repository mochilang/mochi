package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

type Node struct {
	Val int `json:"val"`
	Left int `json:"left"`
	Right int `json:"right"`
}

func lowestCommonAncestor(tree []Node, root int, p int, q int) int {
	var pNode Node = tree[p]
	_ = pNode
	var qNode Node = tree[q]
	_ = qNode
	var pVal int = pNode.Val
	var qVal int = qNode.Val
	var current int = root
	for {
		var node Node = tree[current]
		_ = node
		if ((pVal < node.Val) && (qVal < node.Val)) {
			current = node.Left
		} else 		if ((pVal > node.Val) && (qVal > node.Val)) {
			current = node.Right
		} else {
			return current
		}
	}
}

func example_1() {
	expect((lowestCommonAncestor(example, 0, 1, 2) == 0))
}

func example_2() {
	expect((lowestCommonAncestor(example, 0, 1, 4) == 1))
}

func single_node() {
	var single []Node = []Node{Node{Val: 1, Left: -1, Right: -1}}
	_ = single
	expect((lowestCommonAncestor(single, 0, 0, 0) == 0))
}

var example []Node = []Node{Node{Val: 6, Left: 1, Right: 2}, Node{Val: 2, Left: 3, Right: 4}, Node{Val: 8, Left: 5, Right: 6}, Node{Val: 0, Left: -1, Right: -1}, Node{Val: 4, Left: 7, Right: 8}, Node{Val: 7, Left: -1, Right: -1}, Node{Val: 9, Left: -1, Right: -1}, Node{Val: 3, Left: -1, Right: -1}, Node{Val: 5, Left: -1, Right: -1}}
func main() {
	example_1()
	example_2()
	single_node()
}

