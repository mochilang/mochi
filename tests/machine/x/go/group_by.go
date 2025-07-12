//go:build ignore

package main

import (
	"fmt"
	"mochi/runtime/data"
	"reflect"
	"strings"
)

func main() {
	var people []PeopleItem = []PeopleItem{
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
				g.Key,
				len(g.Items),
				_avg(func() []any {
					results := []any{}
					for _, p := range g.Items {
						results = append(results, _toAnyMap(p)["age"])
					}
					return results
				}()),
			})
		}
		return results
	}()
	fmt.Println("--- People grouped by city ---")
	for _, s := range stats {
		_print(s.City, ": count =", s.Count, ", avg_age =", s.Avg_age)
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

func _print(args ...any) {
	first := true
	for _, a := range args {
		if !first {
			fmt.Print(" ")
		}
		first = false
		rv := reflect.ValueOf(a)
		if a == nil || ((rv.Kind() == reflect.Map || rv.Kind() == reflect.Slice) && rv.IsNil()) {
			fmt.Print("<nil>")
			continue
		}
		if rv.Kind() == reflect.Slice && rv.Type().Elem().Kind() != reflect.Uint8 {
			for i := 0; i < rv.Len(); i++ {
				if i > 0 {
					fmt.Print(" ")
				}
				fmt.Print(_sprint(rv.Index(i).Interface()))
			}
			continue
		}
		fmt.Print(_sprint(a))
	}
	fmt.Println()
}

func _sprint(v any) string {
	if v == nil {
		return "<nil>"
	}
	rv := reflect.ValueOf(v)
	if (rv.Kind() == reflect.Map || rv.Kind() == reflect.Slice) && rv.IsNil() {
		return "<nil>"
	}
	return fmt.Sprint(v)
}

func _toAnyMap(m any) map[string]any {
	switch v := m.(type) {
	case map[string]any:
		return v
	case map[string]string:
		out := make(map[string]any, len(v))
		for k, vv := range v {
			out[k] = vv
		}
		return out
	default:
		rv := reflect.ValueOf(v)
		if rv.Kind() == reflect.Struct {
			out := make(map[string]any, rv.NumField())
			rt := rv.Type()
			for i := 0; i < rv.NumField(); i++ {
				name := rt.Field(i).Name
				if tag := rt.Field(i).Tag.Get("json"); tag != "" {
					comma := strings.Index(tag, ",")
					if comma >= 0 {
						tag = tag[:comma]
					}
					if tag != "-" {
						name = tag
					}
				}
				out[name] = rv.Field(i).Interface()
			}
			return out
		}
		return nil
	}
}
