//go:build ignore

// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z

package main

import (
	"fmt"
	"strings"
)

type v map[string]any

func main() {
	arr1 := []int{
		2,
		7,
		1,
		8,
		2,
	}
	var counts1 map[int]int = map[int]int{}
	var keys1 []int = []int{}
	i := 0
	for i < len(any(arr1)) {
		v := arr1[i]
		key0 := v
		m1 := counts1
		_, ok2 := m1[key0]
		if ok2 {
			counts1[v] = (counts1[v] + 1)
		} else {
			counts1[v] = 1
			keys1 = append(_toAnySlice(keys1), any(v))
		}
		i = (i + 1)
	}
	max1 := 0
	i = 0
	for i < len(any(keys1)) {
		k := keys1[i]
		c := counts1[k]
		if c > max1 {
			max1 = c
		}
		i = (i + 1)
	}
	var modes1 []int = []int{}
	i = 0
	for i < len(any(keys1)) {
		k := keys1[i]
		if counts1[k] == max1 {
			modes1 = append(_toAnySlice(modes1), any(k))
		}
		i = (i + 1)
	}
	fmt.Println(strings.TrimSuffix(fmt.Sprintln(any(fmt.Sprint(any(modes1)))), "\n"))
	arr2 := []int{
		2,
		7,
		1,
		8,
		2,
		8,
	}
	var counts2 map[int]int = map[int]int{}
	var keys2 []int = []int{}
	i = 0
	for i < len(any(arr2)) {
		v := arr2[i]
		key3 := v
		m4 := counts2
		_, ok5 := m4[key3]
		if ok5 {
			counts2[v] = (counts2[v] + 1)
		} else {
			counts2[v] = 1
			keys2 = append(_toAnySlice(keys2), any(v))
		}
		i = (i + 1)
	}
	max2 := 0
	i = 0
	for i < len(any(keys2)) {
		k := keys2[i]
		c := counts2[k]
		if c > max2 {
			max2 = c
		}
		i = (i + 1)
	}
	var modes2 []int = []int{}
	i = 0
	for i < len(any(keys2)) {
		k := keys2[i]
		if counts2[k] == max2 {
			modes2 = append(_toAnySlice(modes2), any(k))
		}
		i = (i + 1)
	}
	fmt.Println(strings.TrimSuffix(fmt.Sprintln(any(fmt.Sprint(any(modes2)))), "\n"))
}

func _toAnySlice[T any](s []T) []any {
	out := make([]any, len(s))
	for i, v := range s {
		out[i] = v
	}
	return out
}
