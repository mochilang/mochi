package main

import (
	"fmt"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func getPermutation(n int, k int) string {
	var factVal int = 1
	for i := 1; i < (n + 1); i++ {
		factVal = (factVal * i)
	}
	var nums []int = []int{}
	var i int = 1
	for (i <= n) {
		nums = append(append([]int{}, nums...), []int{i}...)
		i = (i + 1)
	}
	var k0 int = (k - 1)
	var result string = ""
	i = 0
	for (i < n) {
		factVal = (factVal / ((n - i)))
		var idx int = (k0 / factVal)
		var digit int = nums[idx]
		result = result + fmt.Sprint(digit)
		nums = append(append([]int{}, nums[0:idx]...), nums[(idx + 1):len(nums)]...)
		k0 = (k0 % factVal)
		i = (i + 1)
	}
	return result
}

func example_1() {
	expect((getPermutation(3, 3) == "213"))
}

func example_2() {
	expect((getPermutation(4, 9) == "2314"))
}

func example_3() {
	expect((getPermutation(3, 1) == "123"))
}

func main() {
	example_1()
	example_2()
	example_3()
}

