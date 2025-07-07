package main

import (
	"fmt"
)

func main() {
	var data []int = []int{1, 2}
	var flag bool = len(func() []int {
		_res := []int{}
		for _, x := range data {
			if x == 1 {
				if x == 1 {
					_res = append(_res, x)
				}
			}
		}
		return _res
	}()) > 0
	fmt.Println(flag)
}
