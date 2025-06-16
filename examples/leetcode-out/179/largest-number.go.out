package main

import (
	"fmt"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func largestNumber(nums []int) string {
	var strs []string = []string{}
	var i int = 0
	for (i < len(nums)) {
		strs = append(append([]string{}, strs...), []string{fmt.Sprint(nums[i])}...)
		i = (i + 1)
	}
	var j int = 0
	for (j < len(strs)) {
		var k int = (j + 1)
		for (k < len(strs)) {
			var ab string = strs[j] + strs[k]
			var ba string = strs[k] + strs[j]
			if (ab < ba) {
				var tmp string = strs[j]
				strs[j] = strs[k]
				strs[k] = tmp
			}
			k = (k + 1)
		}
		j = (j + 1)
	}
	var result string = ""
	i = 0
	for (i < len(strs)) {
		result = result + strs[i]
		i = (i + 1)
	}
	var pos int = 0
	for ((pos < (len(result) - 1)) && (_indexString(result, pos) == "0")) {
		pos = (pos + 1)
	}
	return string([]rune(result)[pos:len(result)])
}

func example_1() {
	expect((largestNumber([]int{10, 2}) == "210"))
}

func example_2() {
	expect((largestNumber([]int{3, 30, 34, 5, 9}) == "9534330"))
}

func multiple_zeros() {
	expect((largestNumber([]int{0, 0}) == "0"))
}

func main() {
	example_1()
	example_2()
	multiple_zeros()
}

func _indexString(s string, i int) string {
    runes := []rune(s)
    if i < 0 {
        i += len(runes)
    }
    if i < 0 || i >= len(runes) {
        panic("index out of range")
    }
    return string(runes[i])
}

