//go:build ignore

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
	type Combos struct {
		N any `json:"n"`
		L any `json:"l"`
		B any `json:"b"`
	}

	var combos []Combos = func() []Combos {
		_res := []Combos{}
		for _, n := range nums {
			for _, l := range letters {
				for _, b := range bools {
					_res = append(_res, Combos{
						N: n,
						L: l,
						B: b,
					})
				}
			}
		}
		return _res
	}()
	fmt.Println("--- Cross Join of three lists ---")
	for _, c := range combos {
		fmt.Println(c.N, c.L, c.B)
	}
}
