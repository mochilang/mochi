//go:build ignore

package main

import (
	"fmt"
	"strings"
)

func main() {
	var a []int = []int{1, 2}
	fmt.Println(strings.Trim(fmt.Sprint(append(_convSlice[int, any](a), 3)), "[]"))
}

func _convSlice[T any, U any](s []T) []U {
	out := []U{}
	for _, v := range s {
		out = append(out, any(v).(U))
	}
	return out
}
