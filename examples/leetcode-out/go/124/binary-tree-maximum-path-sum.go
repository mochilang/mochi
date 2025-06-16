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

func max(a int, b int) int {
	if (a > b) {
		return a
	}
	return b
}

func positive(x int) int {
	if (x > 0) {
		return x
	}
	return 0
}

func maxPathSum(root map[string]any) int {
	var best int = -2147483648
	var dfs func(map[string]any) int
	dfs = func(t map[string]any) int {
		if isLeaf(t) {
			return 0
		}
		var leftVal int = dfs(left(t))
		var rightVal int = dfs(right(t))
		var leftPos int = positive(leftVal)
		var rightPos int = positive(rightVal)
		var candidate int = ((value(t) + leftPos) + rightPos)
		if (candidate > best) {
			best = candidate
		}
		return positive((value(t) + max(leftPos, rightPos)))
}
	dfs(root)
	return best
}

func example_1() {
	var tree map[string]any = Node(Node(Leaf(), 2, Leaf()), 1, Node(Leaf(), 3, Leaf()))
	_ = tree
	expect((maxPathSum(tree) == 6))
}

func example_2() {
	var tree map[string]any = Node(Node(Leaf(), 9, Leaf()), -10, Node(Node(Leaf(), 15, Leaf()), 20, Node(Leaf(), 7, Leaf())))
	_ = tree
	expect((maxPathSum(tree) == 42))
}

func single_negative() {
	expect((maxPathSum(Node(Leaf(), -3, Leaf())) == (-3)))
}

func all_negative() {
	var tree map[string]any = Node(Node(Leaf(), -5, Leaf()), -2, Node(Leaf(), -4, Leaf()))
	_ = tree
	expect((maxPathSum(tree) == (-2)))
}

func main() {
	example_1()
	example_2()
	single_negative()
	all_negative()
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

