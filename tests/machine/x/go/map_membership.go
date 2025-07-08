package main

import (
	"fmt"
)

func main() {
	var m map[string]int = map[string]int{"a": 1, "b": 2}
	_tmp0 := "a"
	_tmp1 := m
	_, _tmp2 := _tmp1[_tmp0]
	fmt.Println(_tmp2)
	_tmp3 := "c"
	_tmp4 := m
	_, _tmp5 := _tmp4[_tmp3]
	fmt.Println(_tmp5)
}
