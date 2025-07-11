//go:build ignore

package main

import (
	"fmt"
	"mochi/runtime/data"
)

func main() {
	type PeopleItem struct {
		Name string `json:"name"`
		Age  int    `json:"age"`
		City string `json:"city"`
	}

	var people []PeopleItem = []PeopleItem{
		PeopleItem{
			Name: "Alice",
			Age:  30,
			City: "Paris",
		},
		PeopleItem{
			Name: "Bob",
			Age:  15,
			City: "Hanoi",
		},
		PeopleItem{
			Name: "Charlie",
			Age:  65,
			City: "Paris",
		},
		PeopleItem{
			Name: "Diana",
			Age:  45,
			City: "Hanoi",
		},
		PeopleItem{
			Name: "Eve",
			Age:  70,
			City: "Paris",
		},
		PeopleItem{
			Name: "Frank",
			Age:  22,
			City: "Hanoi",
		},
	}
	type Stats struct {
		City    any     `json:"city"`
		Count   int     `json:"count"`
		Avg_age float64 `json:"avg_age"`
	}

	var stats []Stats = func() []Stats {
		groups := map[string]*data.Group{}
		order := []string{}
		for _, person := range people {
			key := person.City
			ks := fmt.Sprint(key)
			g, ok := groups[ks]
			if !ok {
				g = &data.Group{Key: key}
				groups[ks] = g
				order = append(order, ks)
			}
			g.Items = append(g.Items, person)
		}
		results := []Stats{}
		for _, ks := range order {
			g := groups[ks]
			results = append(results, Stats{
				City:  g.Key,
				Count: len(g.Items),
				Avg_age: _avg(func() []any {
					results := []any{}
					for _, p := range g.Items {
						results = append(results, (p).(map[string]any)["age"])
					}
					return results
				}()),
			})
		}
		return results
	}()
	fmt.Println("--- People grouped by city ---")
	for _, s := range stats {
		fmt.Println(s.City, ": count =", s.Count, ", avg_age =", s.Avg_age)
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
