package main

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

func sortedArrayToBST(nums []int) Tree {
	var helper func(int, int) Tree
	helper = func(lo int, hi int) Tree {
		if (lo > hi) {
			return Leaf{}
		}
		var mid int = (((lo + hi)) / 2)
		return Node{Left: helper(lo, (mid - 1)), Value: nums[mid], Right: helper((mid + 1), hi)}
}
	return helper(0, (len(nums) - 1))
}

func example_1() {
	var tree Tree = sortedArrayToBST([]int{-10, -3, 0, 5, 9})
	_ = tree
	expect(func() bool {
	_t := tree
	if _tmp0, ok := _t.(Node); ok {
		v := _tmp0.Value
		return (v == 0)
	}
	return false
}())
}

func example_2() {
	var tree Tree = sortedArrayToBST([]int{1, 3})
	_ = tree
	expect(func() bool {
	_t := tree
	if _tmp1, ok := _t.(Node); ok {
		v := _tmp1.Value
		return ((v == 1) || (v == 3))
	}
	return false
}())
}

func main() {
	example_1()
	example_2()
}

