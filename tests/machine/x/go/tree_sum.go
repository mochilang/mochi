//go:build ignore

package main

import (
	"fmt"
)

type Tree interface{ isTree() }
type Leaf struct {
}

func (Leaf) isTree() {}

type Node struct {
	Left  Tree `json:"left"`
	Value int  `json:"value"`
	Right Tree `json:"right"`
}

func (Node) isTree() {}

// line 9
func sum_tree(t Tree) int {
	return (func() any {
		_t := t
		if _, ok := _t.(Leaf); ok {
			return 0
		}
		if tmp0, ok := _t.(Node); ok {
			left := tmp0.Left
			value := tmp0.Value
			right := tmp0.Right
			return ((sum_tree(left) + value) + sum_tree(right))
		}
		return nil
	}()).(int)
}

var t Node

func main() {
	t = Node{Tree(Leaf{}), 1, Node{Tree(Leaf{}), 2, Tree(Leaf{})}}
	fmt.Println(sum_tree(t))
}
