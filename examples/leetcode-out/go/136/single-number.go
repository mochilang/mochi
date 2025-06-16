package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func singleNumber(nums []int) int {
	var counts map[int]int = map[int]int{}
	for _, n := range nums {
		_tmp0 := n
		_tmp1 := counts
		_, _tmp2 := _tmp1[_tmp0]
		if _tmp2 {
			counts[n] = (counts[n] + 1)
		} else {
			counts[n] = 1
		}
	}
	for _, n := range nums {
		if (counts[n] == 1) {
			return n
		}
	}
	return 0
}

func example_1() {
	expect((singleNumber([]int{2, 2, 1}) == 1))
}

func example_2() {
	expect((singleNumber([]int{4, 1, 2, 1, 2}) == 4))
}

func example_3() {
	expect((singleNumber([]int{1}) == 1))
}

func with_negatives() {
	expect((singleNumber([]int{4, -1, 2, -1, 2}) == 4))
}

func main() {
	example_1()
	example_2()
	example_3()
	with_negatives()
}

