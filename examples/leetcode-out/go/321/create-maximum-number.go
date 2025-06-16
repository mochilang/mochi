package main

import (
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func pickMax(nums []int, k int) []int {
	var drop int = (len(nums) - k)
	var stack []int = []int{}
	var i int = 0
	var toDrop int = drop
	for (i < len(nums)) {
		var num int = nums[i]
		for ((toDrop > 0) && (len(stack) > 0)) {
			if (stack[(len(stack) - 1)] < num) {
				stack = stack[0:(len(stack) - 1)]
				toDrop = (toDrop - 1)
			} else {
				break
			}
		}
		stack = append(append([]int{}, stack...), []int{num}...)
		i = (i + 1)
	}
	return stack[0:k]
}

func greaterSeq(a []int, i int, b []int, j int) bool {
	var x int = i
	var y int = j
	for ((x < len(a)) && (y < len(b))) {
		if (a[x] > b[y]) {
			return true
		}
		if (a[x] < b[y]) {
			return false
		}
		x = (x + 1)
		y = (y + 1)
	}
	return (x != len(a))
}

func maxNumber(nums1 []int, nums2 []int, k int) []int {
	var best []int = []int{}
	var i int = 0
	for (i <= k) {
		if ((i <= len(nums1)) && ((k - i) <= len(nums2))) {
			var part1 []int = pickMax(nums1, i)
			var part2 []int = pickMax(nums2, (k - i))
			var merged []int = []int{}
			var a int = 0
			var b int = 0
			for ((a < len(part1)) || (b < len(part2))) {
				var use1 bool = false
				if (b == len(part2)) {
					use1 = true
				} else 				if (a == len(part1)) {
					use1 = false
				} else {
					if greaterSeq(part1, a, part2, b) {
						use1 = true
					}
				}
				if use1 {
					merged = append(append([]int{}, merged...), []int{part1[a]}...)
					a = (a + 1)
				} else {
					merged = append(append([]int{}, merged...), []int{part2[b]}...)
					b = (b + 1)
				}
			}
			if ((len(best) == 0) || greaterSeq(merged, 0, best, 0)) {
				best = merged
			}
		}
		i = (i + 1)
	}
	return best
}

func example_1() {
	expect(_equal(maxNumber([]int{3, 4, 6, 5}, []int{9, 1, 2, 5, 8, 3}, 5), []int{9, 8, 6, 5, 3}))
}

func example_2() {
	expect(_equal(maxNumber([]int{6, 7}, []int{6, 0, 4}, 5), []int{6, 7, 6, 0, 4}))
}

func example_3() {
	expect(_equal(maxNumber([]int{3, 9}, []int{8, 9}, 3), []int{9, 8, 9}))
}

func all_from_one() {
	expect(_equal(maxNumber([]int{5, 9, 1}, []int{}, 2), []int{9, 1}))
}

func main() {
	example_1()
	example_2()
	example_3()
	all_from_one()
}

func _equal(a, b any) bool {
    av := reflect.ValueOf(a)
    bv := reflect.ValueOf(b)
    if av.Kind() == reflect.Slice && bv.Kind() == reflect.Slice {
        if av.Len() != bv.Len() { return false }
        for i := 0; i < av.Len(); i++ {
            if !_equal(av.Index(i).Interface(), bv.Index(i).Interface()) { return false }
        }
        return true
    }
    return reflect.DeepEqual(a, b)
}

