package main

import (
	"encoding/json"
	"reflect"
)

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

func levelOrder(root Tree) [][]int {
	if func() bool {
	_t := root
	if _, ok := _t.(Leaf); ok {
		return true
	}
	return false
}() {
		return _cast[[][]int]([]any{})
	}
	var result [][]int = [][]int{}
	var queue []Tree = []Tree{root}
	for (len(queue) > 0) {
		var level []int = []int{}
		var next []Tree = []Tree{}
		for _, node := range queue {
			if func() bool {
	_t := node
	if _, ok := _t.(Leaf); ok {
		return false
	}
	return true
}() {
				level = append(append([]int{}, level...), []int{node.(Node).Value}...)
				if func() bool {
	_t := node.(Node).Left
	if _, ok := _t.(Leaf); ok {
		return false
	}
	return true
}() {
					next = append(append([]Tree{}, next...), []Tree{node.(Node).Left}...)
				}
				if func() bool {
	_t := node.(Node).Right
	if _, ok := _t.(Leaf); ok {
		return false
	}
	return true
}() {
					next = append(append([]Tree{}, next...), []Tree{node.(Node).Right}...)
				}
			}
		}
		result = append(append([][]int{}, result...), [][]int{level}...)
		queue = next
	}
	return result
}

func example_1() {
	var tree Node = Node{Left: Node{Left: Leaf{}, Value: 9, Right: Leaf{}}, Value: 3, Right: Node{Left: Node{Left: Leaf{}, Value: 15, Right: Leaf{}}, Value: 20, Right: Node{Left: Leaf{}, Value: 7, Right: Leaf{}}}}
	_ = tree
	expect(_equal(levelOrder(tree), [][]int{[]int{3}, []int{9, 20}, []int{15, 7}}))
}

func single_node() {
	expect(_equal(levelOrder(Node{Left: Leaf{}, Value: 1, Right: Leaf{}}), [][]int{[]int{1}}))
}

func empty() {
	expect(_equal(levelOrder(Leaf{}), []any{}))
}

func main() {
	example_1()
	single_node()
	empty()
}

func _cast[T any](v any) T {
    data, err := json.Marshal(v)
    if err != nil { panic(err) }
    var out T
    if err := json.Unmarshal(data, &out); err != nil { panic(err) }
    return out
}

func _equal(a, b any) bool {
    av := reflect.ValueOf(a)
    bv := reflect.ValueOf(b)
    if av.Kind() == reflect.Slice && bv.Kind() == reflect.Slice {
        if av.Len() != bv.Len() { return false }
        for i := 0; i < av.Len(); i++ {
            if !_equal(av.Index(i).Interface(), bv.Index(i).Interface()) { return false }
        }
        return true
    }
    return reflect.DeepEqual(a, b)
}

