//go:build ignore

package main

import (
	"fmt"

	"golang.org/x/exp/constraints"
)

func main() {
	fmt.Println(_avgOrdered[int]([]int{1, 2, 3}))
}

func _avgOrdered[T constraints.Integer | constraints.Float](s []T) float64 {
	if len(s) == 0 {
		return 0
	}
	var sum float64
	for _, v := range s {
		sum += float64(v)
	}
	return sum / float64(len(s))
}
