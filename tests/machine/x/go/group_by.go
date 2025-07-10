//go:build ignore

package main

import (
	"fmt"
	"mochi/runtime/data"
	"strings"
)

func main() {
	var people []map[string]any = []map[string]any{
		map[string]any{
			"name": "Alice",
			"age":  30,
			"city": "Paris",
		},
		map[string]any{
			"name": "Bob",
			"age":  15,
			"city": "Hanoi",
		},
		map[string]any{
			"name": "Charlie",
			"age":  65,
			"city": "Paris",
		},
		map[string]any{
			"name": "Diana",
			"age":  45,
			"city": "Hanoi",
		},
		map[string]any{
			"name": "Eve",
			"age":  70,
			"city": "Paris",
		},
		map[string]any{
			"name": "Frank",
			"age":  22,
			"city": "Hanoi",
		},
	}
	var stats []map[string]any = func() []map[string]any {
		groups := map[string]*data.Group{}
		order := []string{}
		for _, person := range people {
			key := person["city"]
			ks := fmt.Sprint(key)
			g, ok := groups[ks]
			if !ok {
				g = &data.Group{Key: key}
				groups[ks] = g
				order = append(order, ks)
			}
			g.Items = append(g.Items, person)
		}
		_res := []map[string]any{}
		for _, ks := range order {
			g := groups[ks]
			_res = append(_res, map[string]any{
				"city":  g.Key,
				"count": len(g.Items),
				"avg_age": _avg(func() []any {
					_res := []any{}
					for _, p := range g.Items {
						_res = append(_res, _cast[map[string]any](p)["age"])
					}
					return _res
				}()),
			})
		}
		return _res
	}()
	fmt.Println("--- People grouped by city ---")
	for _, s := range stats {
		fmt.Println(strings.TrimRight(strings.Join([]string{fmt.Sprint(s["city"]), fmt.Sprint(": count ="), fmt.Sprint(s["count"]), fmt.Sprint(", avg_age ="), fmt.Sprint(s["avg_age"])}, " "), " "))
	}
}

func _avg(v any) float64 {
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
		case []string:
			items = []any{}
			for _, v := range s {
				items = append(items, v)
			}
		case []bool:
			items = []any{}
			for _, v := range s {
				items = append(items, v)
			}
		default:
			panic("avg() expects list or group")
		}
	}
	if len(items) == 0 {
		return 0
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
			panic("avg() expects numbers")
		}
	}
	return sum / float64(len(items))
}

func _cast[T any](v any) T {
	return v.(T)
}

func _convertMapAny(m map[any]any) map[string]any {
	out := make(map[string]any, len(m))
	for k, v := range m {
		key := fmt.Sprint(k)
		if sub, ok := v.(map[any]any); ok {
			out[key] = _convertMapAny(sub)
		} else {
			out[key] = v
		}
	}
	return out
}
