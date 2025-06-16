package main

import (
	"encoding/json"
	"fmt"
	"reflect"
	"sort"
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

func inorder(t Tree) []int {
	return func() []int {
	_t := t
	if _, ok := _t.(Leaf); ok {
		return _cast[[]int]([]any{})
	}
	if _tmp0, ok := _t.(Node); ok {
		l := _tmp0.Left
		v := _tmp0.Value
		r := _tmp0.Right
		return append(append([]int{}, append(append([]int{}, inorder(l)...), []int{v}...)...), inorder(r)...)
	}
	var _zero []int
	return _zero
}()
}

func recoverTree(t Tree) Tree {
	var vals []int = inorder(t)
	var sortedVals []int = func() []int {
	items := []int{}
	for _, x := range vals {
		items = append(items, x)
	}
	type pair struct { item int; key any }
	pairs := make([]pair, len(items))
	for idx, it := range items {
		x := it
		pairs[idx] = pair{item: it, key: x}
	}
	sort.Slice(pairs, func(i, j int) bool {
		a, b := pairs[i].key, pairs[j].key
		switch av := a.(type) {
		case int:
			switch bv := b.(type) {
			case int:
				return av < bv
			case float64:
				return float64(av) < bv
			}
		case float64:
			switch bv := b.(type) {
			case int:
				return av < float64(bv)
			case float64:
				return av < bv
			}
		case string:
			bs, _ := b.(string)
			return av < bs
		}
		return fmt.Sprint(a) < fmt.Sprint(b)
	})
	for idx, p := range pairs {
		items[idx] = p.item
	}
	_res := []int{}
	for _, x := range items {
		_res = append(_res, x)
	}
	return _res
}()
	var build func(int, int) Tree
	build = func(lo int, hi int) Tree {
		if (lo >= hi) {
			return Leaf{}
		}
		var mid int = (((lo + hi)) / 2)
		return Node{Left: build(lo, mid), Value: sortedVals[mid], Right: build((mid + 1), hi)}
}
	return build(0, len(sortedVals))
}

func main() {
	var ex Tree = Node{Left: Node{Left: Leaf{}, Value: 3, Right: Leaf{}}, Value: 1, Right: Node{Left: Leaf{}, Value: 4, Right: Leaf{}}}
	var fixed Tree = recoverTree(ex)
	_ = fixed
	expect(_equal(inorder(fixed), []int{1, 3, 4}))
	var ex2 Tree = Node{Left: Node{Left: Leaf{}, Value: 2, Right: Leaf{}}, Value: 4, Right: Node{Left: Leaf{}, Value: 1, Right: Leaf{}}}
	var fixed2 Tree = recoverTree(ex2)
	_ = fixed2
	expect(_equal(inorder(fixed2), []int{1, 2, 4}))
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

