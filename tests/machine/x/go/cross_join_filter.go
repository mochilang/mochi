//go:build ignore

package main

import (
	"fmt"
	"strings"
)

func main() {
	var nums []int = []int{1, 2, 3}
	var letters []string = []string{"A", "B"}
	_ = letters
	type Pairs struct {
		N any `json:"n"`
		L any `json:"l"`
	}

	var pairs []Pairs = func() []Pairs {
		_res := []Pairs{}
		for _, n := range nums {
			if (n % 2) == 0 {
				for _, l := range letters {
					_res = append(_res, Pairs{
						N: n,
						L: l,
					})
				}
			}
		}
		return _res
	}()
	fmt.Println("--- Even pairs ---")
	for _, p := range pairs {
		fmt.Println(strings.TrimRight(strings.Join([]string{fmt.Sprint(p.N), fmt.Sprint(p.L)}, " "), " "))
	}
}
