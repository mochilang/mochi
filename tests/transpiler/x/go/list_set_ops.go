//go:build ignore

// Generated by Mochi v0.10.36 on 2025-07-22 18:26:34 GMT+7
package main

import (
	"encoding/json"
	"fmt"
	"strings"
)

func main() {
	fmt.Println(func() string {
		b, _ := json.Marshal(func() []int {
			m := map[int]struct{}{}
			res := []int{}
			for _, v := range []int{1, 2} {
				if _, ok := m[v]; !ok {
					m[v] = struct{}{}
					res = append(res, v)
				}
			}
			for _, v := range []int{2, 3} {
				if _, ok := m[v]; !ok {
					m[v] = struct{}{}
					res = append(res, v)
				}
			}
			return res
		}())
		s := string(b)
		s = strings.ReplaceAll(s, ":", ": ")
		s = strings.ReplaceAll(s, ",", ", ")
		s = strings.ReplaceAll(s, "}, {", "},{")
		s = strings.ReplaceAll(s, "\"", "'")
		return s
	}())
	fmt.Println(func() string {
		b, _ := json.Marshal(func() []int {
			m := map[int]struct{}{}
			for _, v := range []int{2} {
				m[v] = struct{}{}
			}
			res := []int{}
			for _, v := range []int{1, 2, 3} {
				if _, ok := m[v]; !ok {
					res = append(res, v)
				}
			}
			return res
		}())
		s := string(b)
		s = strings.ReplaceAll(s, ":", ": ")
		s = strings.ReplaceAll(s, ",", ", ")
		s = strings.ReplaceAll(s, "}, {", "},{")
		s = strings.ReplaceAll(s, "\"", "'")
		return s
	}())
	fmt.Println(func() string {
		b, _ := json.Marshal(func() []int {
			m := map[int]struct{}{}
			for _, v := range []int{1, 2, 3} {
				m[v] = struct{}{}
			}
			res := []int{}
			for _, v := range []int{2, 4} {
				if _, ok := m[v]; ok {
					res = append(res, v)
				}
			}
			return res
		}())
		s := string(b)
		s = strings.ReplaceAll(s, ":", ": ")
		s = strings.ReplaceAll(s, ",", ", ")
		s = strings.ReplaceAll(s, "}, {", "},{")
		s = strings.ReplaceAll(s, "\"", "'")
		return s
	}())
	fmt.Println(len(func() []int {
		res := make([]int, len([]int{1, 2}))
		copy(res, []int{1, 2})
		res = append(res, []int{2, 3}...)
		return res
	}()))
}
