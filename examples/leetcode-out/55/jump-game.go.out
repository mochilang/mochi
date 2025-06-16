package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func canJump(nums []int) bool {
	var n int = len(nums)
	var farthest int = 0
	for i := 0; i < n; i++ {
		if (i > farthest) {
			return false
		}
		if ((i + nums[i]) > farthest) {
			farthest = (i + nums[i])
		}
	}
	return true
}

func example_1() {
	expect((canJump([]int{2, 3, 1, 1, 4}) == true))
}

func example_2() {
	expect((canJump([]int{3, 2, 1, 0, 4}) == false))
}

func single_element() {
	expect((canJump([]int{0}) == true))
}

func with_zeros() {
	expect((canJump([]int{2, 0, 0}) == true))
}

func main() {
	example_1()
	example_2()
	single_element()
	with_zeros()
}

