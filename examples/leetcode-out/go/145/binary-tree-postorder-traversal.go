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

func postorderTraversal(t Tree) []int {
	return func() []int {
	_t := t
	if _, ok := _t.(Leaf); ok {
		return _cast[[]int]([]any{})
	}
	if _tmp0, ok := _t.(Node); ok {
		l := _tmp0.Left
		v := _tmp0.Value
		r := _tmp0.Right
		return append(append([]int{}, append(append([]int{}, postorderTraversal(l)...), postorderTraversal(r)...)...), []int{v}...)
	}
	var _zero []int
	return _zero
}()
}

func example_1() {
	expect(_equal(postorderTraversal(example1), []int{3, 2, 1}))
}

func single_node() {
	expect(_equal(postorderTraversal(Node{Left: Leaf{}, Value: 1, Right: Leaf{}}), []int{1}))
}

func empty() {
	expect(_equal(postorderTraversal(Leaf{}), []any{}))
}

var example1 Node = Node{Left: Leaf{}, Value: 1, Right: Node{Left: Node{Left: Leaf{}, Value: 3, Right: Leaf{}}, Value: 2, Right: Leaf{}}}
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

