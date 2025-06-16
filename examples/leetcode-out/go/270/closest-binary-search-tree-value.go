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

func absFloat(x float64) float64 {
	if (x < 0) {
		return -x
	} else {
		return x
	}
}

func closestValue(root map[string]any, target float64) int {
	var closest int = value(root)
	var node map[string]any = root
	for !isLeaf(node) {
		var v int = value(node)
		if (absFloat(((_cast[float64](v)) - target)) < absFloat(((_cast[float64](closest)) - target))) {
			closest = v
		}
		if (target < (_cast[float64](v))) {
			var l map[string]any = left(node)
			if isLeaf(l) {
				break
			}
			node = l
		} else {
			var r map[string]any = right(node)
			if isLeaf(r) {
				break
			}
			node = r
		}
	}
	return closest
}

func example_1() {
	expect((closestValue(example, 3.714286) == 4))
}

func target_in_tree() {
	expect((closestValue(example, 5) == 5))
}

func target_below_minimum() {
	expect((closestValue(example, 0) == 1))
}

var example map[string]any = Node(Node(Node(Leaf(), 1, Leaf()), 2, Node(Leaf(), 3, Leaf())), 4, Node(Leaf(), 5, Leaf()))
func main() {
	example_1()
	target_in_tree()
	target_below_minimum()
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

