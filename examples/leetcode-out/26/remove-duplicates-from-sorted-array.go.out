package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func removeDuplicates(nums []int) int {
	if (len(nums) == 0) {
		return 0
	}
	var count int = 1
	var prev int = nums[0]
	var i int = 1
	for (i < len(nums)) {
		var cur int = nums[i]
		if (cur != prev) {
			count = (count + 1)
			prev = cur
		}
		i = (i + 1)
	}
	return count
}

func example_1() {
	expect((removeDuplicates([]int{1, 1, 2}) == 2))
}

func example_2() {
	expect((removeDuplicates([]int{0, 0, 1, 1, 1, 2, 2, 3, 3, 4}) == 5))
}

func empty() {
	expect((removeDuplicates([]int{}) == 0))
}

func main() {
	example_1()
	example_2()
	empty()
}

