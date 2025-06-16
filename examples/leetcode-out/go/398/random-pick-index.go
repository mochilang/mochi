package main

import (
	"time"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

type RandomPicker struct {
	Nums []int `json:"nums"`
}

func newPicker(nums []int) RandomPicker {
	return RandomPicker{Nums: nums}
}

func pick(p RandomPicker, target int) int {
	var matches []int = []int{}
	var i int = 0
	for (i < len(p.Nums)) {
		if (p.Nums[i] == target) {
			matches = append(append([]int{}, matches...), []int{i}...)
		}
		i = (i + 1)
	}
	if (len(matches) == 0) {
		return -1
	}
	var idx int64 = (int64(time.Now().UnixNano()) % int64(len(matches)))
	return matches[idx]
}

func example() {
	var p RandomPicker = newPicker([]int{1, 2, 3, 3, 3})
	var idx int = pick(p, 3)
	_ = idx
	expect((((idx == 2) || (idx == 3)) || (idx == 4)))
}

func single_match() {
	var p RandomPicker = newPicker([]int{2, 5, 7})
	_ = p
	expect((pick(p, 5) == 1))
}

func no_match() {
	var p RandomPicker = newPicker([]int{1, 2})
	_ = p
	expect((pick(p, 3) == (-1)))
}

func main() {
	example()
	single_match()
	no_match()
}

