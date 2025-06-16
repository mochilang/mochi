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

func dfs(node map[string]any) map[string]int {
	if isLeaf(node) {
		return map[string]int{"rob": 0, "skip": 0}
	}
	var l map[string]int = dfs(left(node))
	var r map[string]int = dfs(right(node))
	var take int = ((value(node) + l["skip"]) + r["skip"])
	var notake int = l["rob"]
	if (l["skip"] > notake) {
		notake = l["skip"]
	}
	var rr int = r["rob"]
	if (r["skip"] > rr) {
		rr = r["skip"]
	}
	var notake2 int = (notake + rr)
	return map[string]int{"rob": take, "skip": notake2}
}

func rob(root map[string]any) int {
	var res map[string]int = dfs(root)
	var m int = res["rob"]
	if (res["skip"] > m) {
		m = res["skip"]
	}
	return m
}

func example_1() {
	expect((rob(example1) == 7))
}

func example_2() {
	expect((rob(example2) == 9))
}

func single_node() {
	expect((rob(Node(Leaf(), 4, Leaf())) == 4))
}

func empty() {
	expect((rob(Leaf()) == 0))
}

var example1 map[string]any = Node(Node(Leaf(), 2, Node(Leaf(), 3, Leaf())), 3, Node(Leaf(), 3, Node(Leaf(), 1, Leaf())))
var example2 map[string]any = Node(Node(Node(Leaf(), 1, Leaf()), 4, Node(Leaf(), 3, Leaf())), 3, Node(Leaf(), 5, Node(Leaf(), 1, Leaf())))
func main() {
	example_1()
	example_2()
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

