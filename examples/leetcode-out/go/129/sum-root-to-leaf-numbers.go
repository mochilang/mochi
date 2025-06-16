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

func sumNumbers(root map[string]any) int {
	var total int = 0
	var dfs func(map[string]any, int)
	dfs = func(node map[string]any, current int) {
		if !isLeaf(node) {
			var next int = ((current * 10) + value(node))
			var l map[string]any = left(node)
			var r map[string]any = right(node)
			if (isLeaf(l) && isLeaf(r)) {
				total = (total + next)
			} else {
				dfs(l, next)
				dfs(r, next)
			}
		}
}
	dfs(root, 0)
	return total
}

func example_1() {
	var tree map[string]any = Node(Node(Leaf(), 2, Leaf()), 1, Node(Leaf(), 3, Leaf()))
	_ = tree
	expect((sumNumbers(tree) == 25))
}

func example_2() {
	var tree map[string]any = Node(Node(Node(Leaf(), 5, Leaf()), 9, Node(Leaf(), 1, Leaf())), 4, Node(Leaf(), 0, Leaf()))
	_ = tree
	expect((sumNumbers(tree) == 1026))
}

func single_zero() {
	expect((sumNumbers(Node(Leaf(), 0, Leaf())) == 0))
}

func main() {
	example_1()
	example_2()
	single_zero()
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

