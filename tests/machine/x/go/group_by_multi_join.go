//go:build ignore

package main

import (
	"fmt"
	"mochi/runtime/data"
	"reflect"
)

func main() {
	nations := []NationsItem{NationsItem{
		1,
		"A",
	}, NationsItem{
		2,
		"B",
	}}
	_ = nations
	suppliers := []SuppliersItem{SuppliersItem{
		1,
		1,
	}, SuppliersItem{
		2,
		2,
	}}
	_ = suppliers
	partsupp := []PartsuppItem{PartsuppItem{
		100,
		1,
		10.0,
		2,
	}, PartsuppItem{
		100,
		2,
		20.0,
		1,
	}, PartsuppItem{
		200,
		1,
		5.0,
		3,
	}}
	filtered := func() []Filtered {
		results := []Filtered{}
		for _, ps := range partsupp {
			for _, s := range suppliers {
				if !(s.Id == ps.Supplier) {
					continue
				}
				for _, n := range nations {
					if !(n.Id == s.Nation) {
						continue
					}
					if n.Name == "A" {
						if n.Name == "A" {
							results = append(results, Filtered{
								ps.Part,
								(ps.Cost * float64(ps.Qty)),
							})
						}
					}
				}
			}
		}
		return results
	}()
	grouped := func() []Grouped {
		groups := map[string]*data.Group{}
		order := []string{}
		for _, x := range filtered {
			key := x.Part
			ks := fmt.Sprint(key)
			g, ok := groups[ks]
			if !ok {
				g = &data.Group{Key: key}
				groups[ks] = g
				order = append(order, ks)
			}
			g.Items = append(g.Items, x)
		}
		results := []Grouped{}
		for _, ks := range order {
			g := groups[ks]
			results = append(results, Grouped{
				g.Key,
				_sum(func() []any {
					results := []any{}
					for _, r := range g.Items {
						results = append(results, (r).(map[string]any)["value"])
					}
					return results
				}()),
			})
		}
		return results
	}()
	_print(grouped)
}

func _print(args ...any) {
	first := true
	for _, a := range args {
		if !first {
			fmt.Print(" ")
		}
		first = false
		if a == nil {
			fmt.Print("<nil>")
			continue
		}
		rv := reflect.ValueOf(a)
		if (rv.Kind() == reflect.Map || rv.Kind() == reflect.Slice) && rv.IsNil() {
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
