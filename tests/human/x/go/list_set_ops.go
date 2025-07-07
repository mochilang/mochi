//go:build ignore

package main

import (
	"fmt"
)

func union(a, b []int) []int {
	m := make(map[int]bool)
	for _, v := range a {
		m[v] = true
	}
	for _, v := range b {
		if !m[v] {
			a = append(a, v)
			m[v] = true
		}
	}
	return a
}

func except(a, b []int) []int {
	m := make(map[int]bool)
	for _, v := range b {
		m[v] = true
	}
	var r []int
	for _, v := range a {
		if !m[v] {
			r = append(r, v)
		}
	}
	return r
}

func intersect(a, b []int) []int {
	m := make(map[int]bool)
	for _, v := range b {
		m[v] = true
	}
	var r []int
	for _, v := range a {
		if m[v] {
			r = append(r, v)
		}
	}
	return r
}

func unionAll(a, b []int) []int {
	return append(append([]int{}, a...), b...)
}

func main() {
	fmt.Println(union([]int{1, 2}, []int{2, 3}))
	fmt.Println(except([]int{1, 2, 3}, []int{2}))
	fmt.Println(intersect([]int{1, 2, 3}, []int{2, 4}))
	fmt.Println(len(unionAll([]int{1, 2}, []int{2, 3})))
}
