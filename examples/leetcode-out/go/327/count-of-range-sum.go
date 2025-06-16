package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func countRangeSum(nums []int, lower int, upper int) int {
	var n int = len(nums)
	var prefix []int = []int{0}
	var i int = 0
	var running int = 0
	for (i < n) {
		running = (running + nums[i])
		prefix = append(append([]int{}, prefix...), []int{running}...)
		i = (i + 1)
	}
	var count int = 0
	i = 0
	for (i < len(prefix)) {
		var j int = (i + 1)
		for (j < len(prefix)) {
			var sum int = (prefix[j] - prefix[i])
			if ((sum >= lower) && (sum <= upper)) {
				count = (count + 1)
			}
			j = (j + 1)
		}
		i = (i + 1)
	}
	return count
}

func example_1() {
	expect((countRangeSum([]int{-2, 5, -1}, -2, 2) == 3))
}

func example_2() {
	expect((countRangeSum([]int{0}, 0, 0) == 1))
}

func empty() {
	expect((countRangeSum([]int{}, 0, 0) == 0))
}

func single_outside() {
	expect((countRangeSum([]int{3}, -1, 1) == 0))
}

func main() {
	example_1()
	example_2()
	empty()
	single_outside()
}

