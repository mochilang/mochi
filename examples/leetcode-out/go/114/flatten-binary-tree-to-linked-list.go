package main

import (
	"encoding/json"
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func Leaf() map[string]any {
	return _cast[map[string]any](map[string]string{"__name": "Leaf"})
}

func Node(left map[string]any, value int, right map[string]any) map[string]any {
	return map[string]any{"__name": "Node", "left": left, "value": value, "right": right}
}

func isLeaf(t map[string]any) bool {
	return _equal(t["__name"], "Leaf")
}

func left(t map[string]any) map[string]any {
	return t["left"]
}

func right(t map[string]any) map[string]any {
	return t["right"]
}

func value(t map[string]any) int {
	return _cast[int](t["value"])
}

func preorder(t map[string]any) []int {
	if isLeaf(t) {
		return _cast[[]int]([]any{})
	}
	return append(append([]int{}, append(append([]int{}, []int{value(t)}...), preorder(left(t))...)...), preorder(right(t))...)
}

func reverse(xs []int) []int {
	var result []int = []int{}
	for _, x := range xs {
		result = []int{x}
	}
	return result
}

func flatten(root map[string]any) map[string]any {
	var vals []int = reverse(preorder(root))
	var t map[string]any = Leaf()
	for _, v := range vals {
		t = Node(Leaf(), v, t)
	}
	return t
}

func equalTrees(a map[string]any, b map[string]any) bool {
	if (isLeaf(a) && isLeaf(b)) {
		return true
	}
	if (isLeaf(a) || isLeaf(b)) {
		return false
	}
	return (((value(a) == value(b)) && equalTrees(left(a), left(b))) && equalTrees(right(a), right(b)))
}

func example_1() {
	expect((equalTrees(flatten(example1), flattened1) == true))
}

func single_node() {
	var tree map[string]any = Node(Leaf(), 0, Leaf())
	_ = tree
	expect((equalTrees(flatten(tree), tree) == true))
}

func empty() {
	expect((isLeaf(flatten(Leaf())) == true))
}

var example1 map[string]any = Node(Node(Node(Leaf(), 3, Leaf()), 2, Node(Leaf(), 4, Leaf())), 1, Node(Leaf(), 5, Node(Leaf(), 6, Leaf())))
var flattened1 map[string]any = Node(Leaf(), 1, Node(Leaf(), 2, Node(Leaf(), 3, Node(Leaf(), 4, Node(Leaf(), 5, Node(Leaf(), 6, Leaf()))))))
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

