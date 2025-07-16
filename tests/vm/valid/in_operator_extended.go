package main

import (
	"fmt"
	"slices"
	"strings"
)

func main() {
	var xs []int = []int{1, 2, 3}
	var ys []int = func() []int {
		_res := []int{}
		for _, x := range xs {
			if (x % 2) == 1 {
				if (x % 2) == 1 {
					_res = append(_res, x)
				}
			}
		}
		return _res
	}()
	fmt.Println(slices.Contains(ys, 1))
	fmt.Println(slices.Contains(ys, 2))
	var m map[string]int = map[string]int{"a": 1}
	_tmp0 := "a"
	_tmp1 := m
	_, _tmp2 := _tmp1[_tmp0]
	fmt.Println(_tmp2)
	_tmp3 := "b"
	_tmp4 := m
	_, _tmp5 := _tmp4[_tmp3]
	fmt.Println(_tmp5)
	var s string = "hello"
	fmt.Println(strings.Contains(s, "ell"))
	fmt.Println(strings.Contains(s, "foo"))
}
