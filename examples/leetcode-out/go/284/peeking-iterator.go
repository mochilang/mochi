package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

type PeekingIterator struct {
	Nums []int `json:"nums"`
	Index int `json:"index"`
}

type NextResult struct {
	Iter PeekingIterator `json:"iter"`
	Val int `json:"val"`
}

func newPeekingIterator(nums []int) PeekingIterator {
	return PeekingIterator{Nums: nums, Index: 0}
}

func hasNext(it PeekingIterator) bool {
	return (it.Index < len(it.Nums))
}

func peek(it PeekingIterator) int {
	return it.Nums[it.Index]
}

func next(it PeekingIterator) NextResult {
	var v int = it.Nums[it.Index]
	var newIt PeekingIterator = PeekingIterator{Nums: it.Nums, Index: (it.Index + 1)}
	return NextResult{Iter: newIt, Val: v}
}

func example() {
	var it PeekingIterator = newPeekingIterator([]int{1, 2, 3})
	var r1 NextResult = next(it)
	_ = r1
	it = r1.Iter
	expect((r1.Val == 1))
	expect((peek(it) == 2))
	var r2 NextResult = next(it)
	_ = r2
	it = r2.Iter
	expect((r2.Val == 2))
	var r3 NextResult = next(it)
	_ = r3
	it = r3.Iter
	expect((r3.Val == 3))
	expect((hasNext(it) == false))
}

func single_element() {
	var it PeekingIterator = newPeekingIterator([]int{5})
	expect((peek(it) == 5))
	var r NextResult = next(it)
	_ = r
	it = r.Iter
	expect((r.Val == 5))
	expect((hasNext(it) == false))
}

func empty() {
	var it PeekingIterator = newPeekingIterator([]int{})
	_ = it
	expect((hasNext(it) == false))
}

func main() {
	example()
	single_element()
	empty()
}

