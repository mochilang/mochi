package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func maxRotateFunction(nums []int) int {
	var n int = len(nums)
	if (n == 0) {
		return 0
	}
	var total int = 0
	var f int = 0
	var i int = 0
	for (i < n) {
		var val int = nums[i]
		total = (total + val)
		f = (f + (i * val))
		i = (i + 1)
	}
	var best int = f
	var k int = 1
	for (k < n) {
		f = ((f + total) - (n * nums[(n - k)]))
		if (f > best) {
			best = f
		}
		k = (k + 1)
	}
	return best
}

func example() {
	expect((maxRotateFunction([]int{4, 3, 2, 6}) == 26))
}

func single() {
	expect((maxRotateFunction([]int{100}) == 0))
}

func negatives() {
	expect((maxRotateFunction([]int{-1, -2, -3, -4}) == (-12)))
}

func main() {
	example()
	single()
	negatives()
}

