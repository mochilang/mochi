package main

import (
	"encoding/json"
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func Int(val int) map[string]any {
	return map[string]any{"__name": "Int", "value": val}
}

func List(items []map[string]any) map[string]any {
	return map[string]any{"__name": "List", "items": items}
}

func isInt(node map[string]any) bool {
	return _equal(node["__name"], "Int")
}

func value(node map[string]any) int {
	return _cast[int](node["value"])
}

func items(node map[string]any) []map[string]any {
	return _cast[[]map[string]any](node["items"])
}

func depthSum(nested []map[string]any) int {
	var helper func([]map[string]any, int) int
	helper = func(lst []map[string]any, depth int) int {
		var sum int = 0
		for _, node := range lst {
			if isInt(node) {
				sum = (sum + (value(node) * depth))
			} else {
				sum = (sum + helper(items(node), (depth + 1)))
			}
		}
		return sum
}
	return helper(nested, 1)
}

func example_1() {
	expect((depthSum(example1) == 10))
}

func example_2() {
	expect((depthSum(example2) == 27))
}

func empty() {
	expect((depthSum([]map[string]any{}) == 0))
}

var example1 []map[string]any = []map[string]any{List([]map[string]any{Int(1), Int(1)}), Int(2), List([]map[string]any{Int(1), Int(1)})}
var example2 []map[string]any = []map[string]any{Int(1), List([]map[string]any{Int(4), List([]map[string]any{Int(6)})})}
func main() {
	example_1()
	example_2()
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

