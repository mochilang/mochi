package main

import (
	"fmt"
)

func main() {
	var a []int = []int{1, 2}
	fmt.Println(append(_convSlice[int, any](a), 3))
}

func _convSlice[T any, U any](s []T) []U {
	out := []U{}
	for _, v := range s {
		out = append(out, any(v).(U))
	}
	return out
}
