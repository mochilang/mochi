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

func verticalOrder(root map[string]any) [][]int {
	if isLeaf(root) {
		return _cast[[][]int]([]any{})
	}
	var queue []map[string]any = []map[string]any{root}
	var cols []int = []int{0}
	var table map[int][]int = map[int][]int{}
	var minCol int = 0
	var maxCol int = 0
	var i int = 0
	for (i < len(queue)) {
		var node map[string]any = queue[i]
		var col int = cols[i]
		_tmp0 := col
		_tmp1 := table
		_, _tmp2 := _tmp1[_tmp0]
		if _tmp2 {
			table[col] = append(append([]int{}, table[col]...), []int{value(node)}...)
		} else {
			table[col] = []int{value(node)}
		}
		var l map[string]any = left(node)
		var r map[string]any = right(node)
		if !isLeaf(l) {
			queue = append(append([]map[string]any{}, queue...), []map[string]any{l}...)
			cols = append(append([]int{}, cols...), []int{(col - 1)}...)
		}
		if !isLeaf(r) {
			queue = append(append([]map[string]any{}, queue...), []map[string]any{r}...)
			cols = append(append([]int{}, cols...), []int{(col + 1)}...)
		}
		if (col < minCol) {
			minCol = col
		}
		if (col > maxCol) {
			maxCol = col
		}
		i = (i + 1)
	}
	var result [][]int = [][]int{}
	var c int = minCol
	for (c <= maxCol) {
		_tmp3 := c
		_tmp4 := table
		_, _tmp5 := _tmp4[_tmp3]
		if _tmp5 {
			result = append(append([][]int{}, result...), [][]int{table[c]}...)
		}
		c = (c + 1)
	}
	return result
}

func example_1() {
	expect(_equal(verticalOrder(example1), [][]int{[]int{9}, []int{3, 15}, []int{20}, []int{7}}))
}

func example_2() {
	expect(_equal(verticalOrder(example2), [][]int{[]int{4}, []int{2}, []int{1, 5, 6}, []int{3}, []int{7}}))
}

func single_node() {
	expect(_equal(verticalOrder(Node(Leaf(), 1, Leaf())), [][]int{[]int{1}}))
}

func empty() {
	expect(_equal(verticalOrder(Leaf()), []any{}))
}

var example1 map[string]any = Node(Node(Leaf(), 9, Leaf()), 3, Node(Node(Leaf(), 15, Leaf()), 20, Node(Leaf(), 7, Leaf())))
var example2 map[string]any = Node(Node(Node(Leaf(), 4, Leaf()), 2, Node(Leaf(), 5, Leaf())), 1, Node(Node(Leaf(), 6, Leaf()), 3, Node(Leaf(), 7, Leaf())))
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

