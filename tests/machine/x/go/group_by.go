//go:build ignore

package main

import (
	"fmt"
	"mochi/runtime/data"
)

func main() {
	people := []PeopleItem{
		PeopleItem{
			"Alice",
			30,
			"Paris",
		},
		PeopleItem{
			"Bob",
			15,
			"Hanoi",
		},
		PeopleItem{
			"Charlie",
			65,
			"Paris",
		},
		PeopleItem{
			"Diana",
			45,
			"Hanoi",
		},
		PeopleItem{
			"Eve",
			70,
			"Paris",
		},
		PeopleItem{
			"Frank",
			22,
			"Hanoi",
		},
	}
	stats := func() []Stats {
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
				g.Key,
				len(g.Items),
				_avg(func() []any {
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
