//go:build ignore

package main

import (
	"fmt"
	"reflect"
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
		if _tmp0, ok := _t.(Node); ok {
			left := _tmp0.Left
			value := _tmp0.Value
			right := _tmp0.Right
			return ((sum_tree(left) + value) + sum_tree(right))
		}
		return nil
	}()).(int)
}

var t Node

func main() {
	t = Node{Left: Tree(Leaf{}), Value: 1, Right: Node{Left: Tree(Leaf{}), Value: 2, Right: Tree(Leaf{})}}
	fmt.Println(_sprint(sum_tree(t)))
}

func _sprint(v any) string {
	if v == nil {
		return "<nil>"
	}
	rv := reflect.ValueOf(v)
	if (rv.Kind() == reflect.Map || rv.Kind() == reflect.Slice) && rv.IsNil() {
		return "<nil>"
	}
	return fmt.Sprint(v)
}
