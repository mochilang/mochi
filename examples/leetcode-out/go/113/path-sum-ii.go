package main

import (
	"encoding/json"
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

func pathSum(root Tree, targetSum int) [][]int {
	var dfs func(Tree, int, []int) [][]int
	dfs = func(node Tree, remaining int, path []int) [][]int {
		var handle = func(l Tree, v int, r Tree, rem int, p []int) [][]int {
			var leftEmpty bool = func() bool {
			_t := l
			if _, ok := _t.(Leaf); ok {
				return true
			}
			return false
		}()
			var rightEmpty bool = func() bool {
			_t := r
			if _, ok := _t.(Leaf); ok {
				return true
			}
			return false
		}()
			var newRemaining int = (rem - v)
			var newPath []int = append(append([]int{}, p...), []int{v}...)
			if (leftEmpty && rightEmpty) {
				if (newRemaining == 0) {
					return [][]int{newPath}
				} else {
					return _cast[[][]int]([]any{})
				}
			}
			return append(append([][]int{}, dfs(l, newRemaining, newPath)...), dfs(r, newRemaining, newPath)...)
	}
		return func() [][]int {
		_t := node
		if _, ok := _t.(Leaf); ok {
			return _cast[[][]int]([]any{})
		}
		if _tmp0, ok := _t.(Node); ok {
			l := _tmp0.Left
			v := _tmp0.Value
			r := _tmp0.Right
			return handle(l, v, r, remaining, path)
		}
		var _zero [][]int
		return _zero
	}()
}
	return dfs(root, targetSum, []int{})
}

func example_1() {
	var rootLeft Node = Node{Left: Node{Left: Leaf{}, Value: 7, Right: Leaf{}}, Value: 11, Right: Node{Left: Leaf{}, Value: 2, Right: Leaf{}}}
	var tree Node = Node{Left: Node{Left: rootLeft, Value: 4, Right: Leaf{}}, Value: 5, Right: Node{Left: Node{Left: Leaf{}, Value: 13, Right: Leaf{}}, Value: 8, Right: Node{Left: Node{Left: Leaf{}, Value: 5, Right: Leaf{}}, Value: 4, Right: Node{Left: Leaf{}, Value: 1, Right: Leaf{}}}}}
	_ = tree
	expect(_equal(pathSum(tree, 22), [][]int{[]int{5, 4, 11, 2}, []int{5, 8, 4, 5}}))
}

func example_2() {
	var tree Node = Node{Left: Node{Left: Leaf{}, Value: 2, Right: Leaf{}}, Value: 1, Right: Node{Left: Leaf{}, Value: 3, Right: Leaf{}}}
	_ = tree
	expect(_equal(pathSum(tree, 5), []any{}))
}

func example_3() {
	var tree Node = Node{Left: Node{Left: Leaf{}, Value: 2, Right: Leaf{}}, Value: 1, Right: Leaf{}}
	_ = tree
	expect(_equal(pathSum(tree, 0), []any{}))
}

func main() {
	example_1()
	example_2()
	example_3()
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

