//go:build ignore

package main

import (
	"fmt"
	"mochi/runtime/data"
)

func main() {
	var nums []int = []int{1, 2, 3}
	var result []float64 = func() []float64 {
		_res := []float64{}
		for _, n := range nums {
			if n > 1 {
				if n > 1 {
					_res = append(_res, _sum(n))
				}
			}
		}
		return _res
	}()
	fmt.Println(result)
}

func _sum(v any) float64 {
	var items []any
	if g, ok := v.(*data.Group); ok {
		items = g.Items
	} else {
		switch s := v.(type) {
		case []any:
			items = s
		case []int:
			items = []any{}
			for _, v := range s {
				items = append(items, v)
			}
		case []float64:
			items = []any{}
			for _, v := range s {
				items = append(items, v)
			}
		case []string, []bool:
			panic("sum() expects numbers")
		default:
			panic("sum() expects list or group")
		}
	}
	var sum float64
	for _, it := range items {
		switch n := it.(type) {
		case int:
			sum += float64(n)
		case int64:
			sum += float64(n)
		case float64:
			sum += n
		default:
			panic("sum() expects numbers")
		}
	}
	return sum
}
