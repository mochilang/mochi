//go:build ignore

package main

import (
	"fmt"
)

func main() {
	dataVar := []int{1, 2}
	flag := len(func() []int {
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
