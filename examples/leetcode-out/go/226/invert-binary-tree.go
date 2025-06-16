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

func invertTree(root map[string]any) map[string]any {
	if isLeaf(root) {
		return root
	}
	var l map[string]any = left(root)
	var r map[string]any = right(root)
	return Node(invertTree(r), value(root), invertTree(l))
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

func invert_small_tree() {
	expect((equalTrees(invertTree(example), inverted) == true))
}

func single_node() {
	var t map[string]any = Node(Leaf(), 1, Leaf())
	_ = t
	expect((equalTrees(invertTree(t), t) == true))
}

func empty_tree() {
	expect((isLeaf(invertTree(Leaf())) == true))
}

var example map[string]any = Node(Node(Leaf(), 2, Leaf()), 1, Node(Leaf(), 3, Leaf()))
var inverted map[string]any = Node(Node(Leaf(), 3, Leaf()), 1, Node(Leaf(), 2, Leaf()))
func main() {
	invert_small_tree()
	single_node()
	empty_tree()
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

