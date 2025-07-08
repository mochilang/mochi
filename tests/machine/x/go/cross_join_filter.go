//go:build ignore

package main

import (
	"fmt"
)

func main() {
	var nums []int = []int{1, 2, 3}
	var letters []string = []string{"A", "B"}
	_ = letters
	var pairs []map[string]any = func() []map[string]any {
		_res := []map[string]any{}
		for _, n := range nums {
			if (n % 2) == 0 {
				for _, l := range letters {
					_res = append(_res, map[string]any{"n": n, "l": l})
				}
			}
		}
		return _res
	}()
	fmt.Println("--- Even pairs ---")
	for _, p := range pairs {
		fmt.Println(p["n"], p["l"])
	}
}
