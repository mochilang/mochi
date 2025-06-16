package main

import (
	"encoding/json"
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

type NestedIterator struct {
	Data []int `json:"data"`
	Index int `json:"index"`
}

type NextResult struct {
	It NestedIterator `json:"it"`
	Val int `json:"val"`
}

func IntItem(v int) map[string]any {
	return map[string]any{"kind": "int", "value": v}
}

func ListItem(items []map[string]any) map[string]any {
	return map[string]any{"kind": "list", "value": items}
}

func isInt(item map[string]any) bool {
	return _equal(item["kind"], "int")
}

func itemValue(item map[string]any) int {
	return _cast[int](item["value"])
}

func itemList(item map[string]any) []map[string]any {
	return _cast[[]map[string]any](item["value"])
}

func flatten(items []map[string]any) []int {
	var result []int = []int{}
	var i int = 0
	for (i < len(items)) {
		var it map[string]any = items[i]
		if isInt(it) {
			result = append(append([]int{}, result...), []int{itemValue(it)}...)
		} else {
			result = append(append([]int{}, result...), flatten(itemList(it))...)
		}
		i = (i + 1)
	}
	return result
}

func newNestedIterator(nested []map[string]any) NestedIterator {
	return NestedIterator{Data: flatten(nested), Index: 0}
}

func nestedHasNext(it NestedIterator) bool {
	return (it.Index < len(it.Data))
}

func nestedNext(it NestedIterator) NextResult {
	var value int = it.Data[it.Index]
	return NextResult{It: NestedIterator{Data: it.Data, Index: (it.Index + 1)}, Val: value}
}

func example_1() {
	var nested []map[string]any = []map[string]any{ListItem([]map[string]any{IntItem(1), IntItem(1)}), IntItem(2), ListItem([]map[string]any{IntItem(1), IntItem(1)})}
	var it NestedIterator = newNestedIterator(nested)
	var r1 NextResult = nestedNext(it)
	_ = r1
	it = r1.It
	expect((r1.Val == 1))
	var r2 NextResult = nestedNext(it)
	_ = r2
	it = r2.It
	expect((r2.Val == 1))
	var r3 NextResult = nestedNext(it)
	_ = r3
	it = r3.It
	expect((r3.Val == 2))
	var r4 NextResult = nestedNext(it)
	_ = r4
	it = r4.It
	expect((r4.Val == 1))
	var r5 NextResult = nestedNext(it)
	_ = r5
	it = r5.It
	expect((r5.Val == 1))
	expect((nestedHasNext(it) == false))
}

func example_2() {
	var nested []map[string]any = []map[string]any{IntItem(1), ListItem([]map[string]any{IntItem(4), ListItem([]map[string]any{IntItem(6)})})}
	var it NestedIterator = newNestedIterator(nested)
	var r1 NextResult = nestedNext(it)
	_ = r1
	it = r1.It
	expect((r1.Val == 1))
	var r2 NextResult = nestedNext(it)
	_ = r2
	it = r2.It
	expect((r2.Val == 4))
	var r3 NextResult = nestedNext(it)
	_ = r3
	it = r3.It
	expect((r3.Val == 6))
	expect((nestedHasNext(it) == false))
}

func empty_list() {
	var it NestedIterator = newNestedIterator([]map[string]any{})
	_ = it
	expect((nestedHasNext(it) == false))
}

func empty_inner_lists() {
	var nested []map[string]any = []map[string]any{ListItem([]map[string]any{}), IntItem(3), ListItem([]map[string]any{})}
	var it NestedIterator = newNestedIterator(nested)
	var r NextResult = nestedNext(it)
	_ = r
	it = r.It
	expect((r.Val == 3))
	expect((nestedHasNext(it) == false))
}

func main() {
	example_1()
	example_2()
	empty_list()
	empty_inner_lists()
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

