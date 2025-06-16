package main

import (
	"encoding/json"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

type MedianFinder struct {
	Values []int `json:"values"`
}

func newFinder() MedianFinder {
	return MedianFinder{Values: _cast[[]int]([]any{})}
}

func addNum(mf MedianFinder, num int) MedianFinder {
	var vals []int = mf.Values
	var i int = 0
	for (i < len(vals)) {
		if (vals[i] < num) {
			i = (i + 1)
		} else {
			break
		}
	}
	vals = append(append([]int{}, append(append([]int{}, vals[0:i]...), []int{num}...)...), vals[i:len(vals)]...)
	return MedianFinder{Values: vals}
}

func findMedian(mf MedianFinder) float64 {
	var n int = len(mf.Values)
	if ((n % 2) == 1) {
		return (_cast[float64](mf.Values[(n / 2)]))
	}
	var a int = mf.Values[((n / 2) - 1)]
	var b int = mf.Values[(n / 2)]
	return ((((_cast[float64](a)) + (_cast[float64](b)))) / 2)
}

func example() {
	var mf MedianFinder = newFinder()
	mf = addNum(mf, 1)
	mf = addNum(mf, 2)
	expect((findMedian(mf) == 1.5))
	mf = addNum(mf, 3)
	expect((findMedian(mf) == 2))
}

func single_value() {
	var mf MedianFinder = newFinder()
	mf = addNum(mf, 5)
	expect((findMedian(mf) == 5))
}

func even_count() {
	var mf MedianFinder = newFinder()
	mf = addNum(mf, 2)
	mf = addNum(mf, 4)
	mf = addNum(mf, 6)
	mf = addNum(mf, 8)
	expect((findMedian(mf) == 5))
}

func main() {
	example()
	single_value()
	even_count()
}

func _cast[T any](v any) T {
    data, err := json.Marshal(v)
    if err != nil { panic(err) }
    var out T
    if err := json.Unmarshal(data, &out); err != nil { panic(err) }
    return out
}

