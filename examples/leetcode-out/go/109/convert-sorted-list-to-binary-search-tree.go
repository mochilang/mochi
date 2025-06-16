package main

import (
	"reflect"
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

func sortedListToBST(nums []int) Tree {
	var build func(int, int) Tree
	build = func(lo int, hi int) Tree {
		if (lo >= hi) {
			return Leaf{}
		}
		var mid int = (((lo + hi)) / 2)
		return Node{Left: build(lo, mid), Value: nums[mid], Right: build((mid + 1), hi)}
}
	return build(0, len(nums))
}

func inorder(t Tree) []int {
	return func() any {
	_t := t
	if _, ok := _t.(Leaf); ok {
		return []any{}
	}
	if _tmp0, ok := _t.(Node); ok {
		l := _tmp0.Left
		v := _tmp0.Value
		r := _tmp0.Right
		return append(append([]int{}, append(append([]int{}, inorder(l)...), []int{v}...)...), inorder(r)...)
	}
	return nil
}()
}

func example() {
	var nums []int = []int{-10, -3, 0, 5, 9}
	var tree Tree = sortedListToBST(nums)
	_ = tree
	expect(_equal(inorder(tree), nums))
}

func empty() {
	expect(_equal(inorder(sortedListToBST([]int{})), []any{}))
}

func single() {
	expect(_equal(inorder(sortedListToBST([]int{1})), []int{1}))
}

func main() {
	example()
	empty()
	single()
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

