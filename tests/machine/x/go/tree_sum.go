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
	return func() any {
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
	}().(int)
}

var t Node

func main() {
	t = Node{Left: _cast[Tree](Leaf{}), Value: 1, Right: Node{Left: _cast[Tree](Leaf{}), Value: 2, Right: _cast[Tree](Leaf{})}}
	fmt.Println(sum_tree(t))
}

func _cast[T any](v any) T {
	return v.(T)
}

func _convertMapAny(m map[any]any) map[string]any {
	out := make(map[string]any, len(m))
	for k, v := range m {
		key := fmt.Sprint(k)
		if sub, ok := v.(map[any]any); ok {
			out[key] = _convertMapAny(sub)
		} else {
			out[key] = v
		}
	}
	return out
}
