package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func maxSubArrayLen(nums []int, k int) int {
	var prefix int = 0
	var firstIndex map[int]int = map[int]int{}
	firstIndex[0] = -1
	var best int = 0
	var i int = 0
	for (i < len(nums)) {
		prefix = (prefix + nums[i])
		var target int = (prefix - k)
		_tmp0 := target
		_tmp1 := firstIndex
		_, _tmp2 := _tmp1[_tmp0]
		if _tmp2 {
			var start int = firstIndex[target]
			var length int = (i - start)
			if (length > best) {
				best = length
			}
		}
		_tmp3 := prefix
		_tmp4 := firstIndex
		_, _tmp5 := _tmp4[_tmp3]
		if !(_tmp5) {
			firstIndex[prefix] = i
		}
		i = (i + 1)
	}
	return best
}

func example_1() {
	expect((maxSubArrayLen([]int{1, -1, 5, -2, 3}, 3) == 4))
}

func example_2() {
	expect((maxSubArrayLen([]int{-2, -1, 2, 1}, 1) == 2))
}

func no_subarray() {
	expect((maxSubArrayLen([]int{1, 2, 3}, 7) == 0))
}

func entire_array() {
	expect((maxSubArrayLen([]int{1, 2, 3}, 6) == 3))
}

func single_negative() {
	expect((maxSubArrayLen([]int{-1}, -1) == 1))
}

func main() {
	example_1()
	example_2()
	no_subarray()
	entire_array()
	single_negative()
}

