package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func missingNumber(nums []int) int {
	var n int = len(nums)
	var sum int = 0
	for _, num := range nums {
		sum = (sum + num)
	}
	var expected int = ((n * ((n + 1))) / 2)
	return (expected - sum)
}

func example_1() {
	expect((missingNumber([]int{3, 0, 1}) == 2))
}

func example_2() {
	expect((missingNumber([]int{0, 1}) == 2))
}

func example_3() {
	expect((missingNumber([]int{9, 6, 4, 2, 3, 5, 7, 0, 1}) == 8))
}

func single_zero() {
	expect((missingNumber([]int{0}) == 1))
}

func already_ordered() {
	expect((missingNumber([]int{0, 2, 1, 4, 5, 6, 7, 8, 9}) == 3))
}

func main() {
	example_1()
	example_2()
	example_3()
	single_zero()
	already_ordered()
}

