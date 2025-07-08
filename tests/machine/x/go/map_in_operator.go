//go:build ignore

package main

import (
	"fmt"
)

func main() {
	var m map[int]string = map[int]string{1: "a", 2: "b"}
	_tmp0 := 1
	_tmp1 := m
	_, _tmp2 := _tmp1[_tmp0]
	fmt.Println(_tmp2)
	_tmp3 := 3
	_tmp4 := m
	_, _tmp5 := _tmp4[_tmp3]
	fmt.Println(_tmp5)
}
