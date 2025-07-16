package main

import (
	"fmt"
)

func main() {
	var nums []int = []int{1, 2}
	var letters []string = []string{"A", "B"}
	_ = letters
	var bools []bool = []bool{true, false}
	_ = bools
	var combos []map[string]any = func() []map[string]any {
		_res := []map[string]any{}
		for _, n := range nums {
			for _, l := range letters {
				for _, b := range bools {
					_res = append(_res, map[string]any{
						"n": n,
						"l": l,
						"b": b,
					})
				}
			}
		}
		return _res
	}()
	fmt.Println("--- Cross Join of three lists ---")
	for _, c := range combos {
		fmt.Println(c["n"], c["l"], c["b"])
	}
}
