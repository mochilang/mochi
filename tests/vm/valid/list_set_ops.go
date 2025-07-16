package main

import (
	"fmt"
)

func main() {
	fmt.Println(_union[int]([]int{1, 2}, []int{2, 3}))
	fmt.Println(_except[int]([]int{1, 2, 3}, []int{2}))
	fmt.Println(_intersect[int]([]int{1, 2, 3}, []int{2, 4}))
	fmt.Println(len(_union[int]([]int{1, 2}, []int{2, 3})))
}

func _except[T any](a, b []T) []T {
	res := []T{}
	for _, x := range a {
		keep := true
		for _, y := range b {
			if _equal(x, y) {
				keep = false
				break
			}
		}
		if keep {
			res = append(res, x)
		}
	}
	return res
}

func _intersect[T any](a, b []T) []T {
	res := []T{}
	for _, x := range a {
		inB := false
		for _, y := range b {
			if _equal(x, y) {
				inB = true
				break
			}
		}
		if inB {
			exists := false
			for _, r := range res {
				if _equal(x, r) {
					exists = true
					break
				}
			}
			if !exists {
				res = append(res, x)
			}
		}
	}
	return res
}

func _union[T any](a, b []T) []T {
	res := append([]T{}, a...)
	for _, it := range b {
		found := false
		for _, v := range res {
			if _equal(v, it) {
				found = true
				break
			}
		}
		if !found {
			res = append(res, it)
		}
	}
	return res
}
