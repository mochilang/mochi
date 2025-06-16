package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func wiggleMaxLength(nums []int) int {
	var n int = len(nums)
	if (n < 2) {
		return n
	}
	var up int = 1
	var down int = 1
	for i := 1; i < n; i++ {
		if (nums[i] > nums[(i - 1)]) {
			up = (down + 1)
		} else 		if (nums[i] < nums[(i - 1)]) {
			down = (up + 1)
		}
	}
	if (up > down) {
		return up
	}
	return down
}

func example_1() {
	expect((wiggleMaxLength([]int{1, 7, 4, 9, 2, 5}) == 6))
}

func example_2() {
	expect((wiggleMaxLength([]int{1, 17, 5, 10, 13, 15, 10, 5, 16, 8}) == 7))
}

func example_3() {
	expect((wiggleMaxLength([]int{1, 2, 3, 4, 5, 6, 7, 8, 9}) == 2))
}

func single_element() {
	expect((wiggleMaxLength([]int{5}) == 1))
}

func two_equal() {
	expect((wiggleMaxLength([]int{3, 3}) == 1))
}

func main() {
	example_1()
	example_2()
	example_3()
	single_element()
	two_equal()
}

