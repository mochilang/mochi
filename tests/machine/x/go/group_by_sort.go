//go:build ignore

package main

import (
	"fmt"
	"mochi/runtime/data"
	"reflect"
	"sort"
)

func main() {
	items := []ItemsItem{
		ItemsItem{
			"a",
			3,
		},
		ItemsItem{
			"a",
			1,
		},
		ItemsItem{
			"b",
			5,
		},
		ItemsItem{
			"b",
			2,
		},
	}
	grouped := func() []Grouped {
		groups := map[string]*data.Group{}
		order := []string{}
		for _, i := range items {
			key := i.Cat
			ks := fmt.Sprint(key)
			g, ok := groups[ks]
			if !ok {
				g = &data.Group{Key: key}
				groups[ks] = g
				order = append(order, ks)
			}
			_item := map[string]any{}
			_item["cat"] = i.Cat
			_item["val"] = i.Val
			_item["i"] = i
			g.Items = append(g.Items, _item)
		}
		items := []*data.Group{}
		for _, ks := range order {
			items = append(items, groups[ks])
		}
		type pair struct {
			item *data.Group
			key  any
		}
		pairs := make([]pair, len(items))
		for idx, it := range items {
			g := it
			pairs[idx] = pair{item: it, key: -_sum(func() []any {
				results := []any{}
				for _, x := range g.Items {
					results = append(results, (x).(map[string]any)["val"])
				}
				return results
			}())}
		}
		sort.Slice(pairs, func(i, j int) bool {
			a, b := pairs[i].key, pairs[j].key
			switch av := a.(type) {
			case int:
				switch bv := b.(type) {
				case int:
					return av < bv
				case float64:
					return float64(av) < bv
				}
			case float64:
				switch bv := b.(type) {
				case int:
					return av < float64(bv)
				case float64:
					return av < bv
				}
			case string:
				bs, _ := b.(string)
				return av < bs
			}
			return fmt.Sprint(a) < fmt.Sprint(b)
		})
		for idx, p := range pairs {
			items[idx] = p.item
		}
		results := []Grouped{}
		for _, g := range items {
			results = append(results, Grouped{
				g.Key,
				_sum(func() []any {
					results := []any{}
					for _, x := range g.Items {
						results = append(results, (x).(map[string]any)["val"])
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
