//go:build ignore

package main

import (
	"fmt"
)

func main() {
	var dataVar []int = []int{1, 2}
	var flag bool = len(func() []int {
		results := []int{}
		for _, x := range dataVar {
			if x == 1 {
				if x == 1 {
					results = append(results, x)
				}
			}
		}
		return results
	}()) > 0
	fmt.Println(flag)
}
