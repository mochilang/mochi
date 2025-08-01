//go:build ignore

// Generated by Mochi compiler v0.10.28 on 1970-01-01T00:00:00Z

package main

import (
	"fmt"
	"slices"
	"strings"
)

type v map[string]any

func main() {
	xs := []int{1, 2, 3}
	ys := func() []int {
		results := []int{}
		for _, x := range xs {
			if (x % 2) == 1 {
				if (x % 2) == 1 {
					results = append(results, x)
				}
			}
		}
		return results
	}()
	fmt.Println(strings.TrimSpace(fmt.Sprintln(func() int {
		if any(slices.Contains(ys, 1)) {
			return 1
		}
		return 0
	}())))
	fmt.Println(strings.TrimSpace(fmt.Sprintln(func() int {
		if any(slices.Contains(ys, 2)) {
			return 1
		}
		return 0
	}())))
	m := map[string]int{"a": 1}
	key0 := "a"
	m1 := m
	_, ok2 := m1[key0]
	fmt.Println(strings.TrimSpace(fmt.Sprintln(func() int {
		if any(ok2) {
			return 1
		}
		return 0
	}())))
	key3 := "b"
	m4 := m
	_, ok5 := m4[key3]
	fmt.Println(strings.TrimSpace(fmt.Sprintln(func() int {
		if any(ok5) {
			return 1
		}
		return 0
	}())))
	s := "hello"
	fmt.Println(strings.TrimSpace(fmt.Sprintln(func() int {
		if any(strings.Contains(s, "ell")) {
			return 1
		}
		return 0
	}())))
	fmt.Println(strings.TrimSpace(fmt.Sprintln(func() int {
		if any(strings.Contains(s, "foo")) {
			return 1
		}
		return 0
	}())))
}
