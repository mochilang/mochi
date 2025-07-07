//go:build ignore

package main

import "fmt"

// Tree represents a binary tree of integers.
type Tree interface{}

type Leaf struct{}

// Node is a tree node with left and right subtrees and a value.
type Node struct {
	Left  Tree
	Value int
	Right Tree
}

func sumTree(t Tree) int {
	switch v := t.(type) {
	case Leaf:
		return 0
	case Node:
		return sumTree(v.Left) + v.Value + sumTree(v.Right)
	default:
		return 0
	}
}

func main() {
	t := Node{Left: Leaf{}, Value: 1, Right: Node{Left: Leaf{}, Value: 2, Right: Leaf{}}}
	fmt.Println(sumTree(t))
}
