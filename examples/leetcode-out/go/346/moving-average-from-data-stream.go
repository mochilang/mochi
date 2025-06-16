package main

import (
	"encoding/json"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

type MovingAverage struct {
	Size int `json:"size"`
	Nums []int `json:"nums"`
	Total int `json:"total"`
}

type NextResult struct {
	Ma MovingAverage `json:"ma"`
	Average float64 `json:"average"`
}

func newMovingAverage(size int) MovingAverage {
	return MovingAverage{Size: size, Nums: _cast[[]int]([]any{}), Total: 0}
}

func next(ma MovingAverage, val int) NextResult {
	var nums []int = ma.Nums
	var total int = ma.Total
	if (len(nums) == ma.Size) {
		total = (total - nums[0])
		nums = nums[1:len(nums)]
	}
	nums = append(append([]int{}, nums...), []int{val}...)
	total = (total + val)
	var updated MovingAverage = MovingAverage{Size: ma.Size, Nums: nums, Total: total}
	var avg float64 = ((_cast[float64](total)) / (_cast[float64](len(nums))))
	return NextResult{Ma: updated, Average: avg}
}

func example() {
	var ma MovingAverage = newMovingAverage(3)
	var r1 NextResult = next(ma, 1)
	_ = r1
	ma = r1.Ma
	expect((r1.Average == 1))
	var r2 NextResult = next(ma, 10)
	_ = r2
	ma = r2.Ma
	expect((r2.Average == 5.5))
	var r3 NextResult = next(ma, 3)
	_ = r3
	ma = r3.Ma
	expect((r3.Average == (14 / 3)))
	var r4 NextResult = next(ma, 5)
	_ = r4
	ma = r4.Ma
	expect((r4.Average == 6))
}

func single_element_window() {
	var ma MovingAverage = newMovingAverage(1)
	var r NextResult = next(ma, 4)
	_ = r
	ma = r.Ma
	expect((r.Average == 4))
	var r2 NextResult = next(ma, 7)
	_ = r2
	ma = r2.Ma
	expect((r2.Average == 7))
}

func window_smaller_than_inputs() {
	var ma MovingAverage = newMovingAverage(2)
	var a NextResult = next(ma, 3)
	_ = a
	ma = a.Ma
	expect((a.Average == 3))
	var b NextResult = next(ma, 5)
	_ = b
	ma = b.Ma
	expect((b.Average == 4))
	var c NextResult = next(ma, 7)
	_ = c
	ma = c.Ma
	expect((c.Average == 6))
}

func main() {
	example()
	single_element_window()
	window_smaller_than_inputs()
}

func _cast[T any](v any) T {
    data, err := json.Marshal(v)
    if err != nil { panic(err) }
    var out T
    if err := json.Unmarshal(data, &out); err != nil { panic(err) }
    return out
}

