//go:build ignore

package main

import (
	"fmt"
	"mochi/runtime/data"
	"reflect"
)

func main() {
	customers := []CustomersItem{CustomersItem{
		1,
		"Alice",
	}, CustomersItem{
		2,
		"Bob",
	}, CustomersItem{
		3,
		"Charlie",
	}}
	orders := []OrdersItem{OrdersItem{
		100,
		1,
	}, OrdersItem{
		101,
		1,
	}, OrdersItem{
		102,
		2,
	}}
	_ = orders
	stats := func() []Stats {
		groups := map[string]*data.Group{}
		order := []string{}
		for _, c := range customers {
			matched := false
			for _, o := range orders {
				if !(o.CustomerId == c.Id) {
					continue
				}
				matched = true
				key := c.Name
				ks := fmt.Sprint(key)
				g, ok := groups[ks]
				if !ok {
					g = &data.Group{Key: key}
					groups[ks] = g
					order = append(order, ks)
				}
				_item := map[string]any{}
				_item["id"] = c.Id
				_item["name"] = c.Name
				_item["c"] = c
				_item["id"] = o.Id
				_item["customerId"] = o.CustomerId
				_item["o"] = o
				g.Items = append(g.Items, _item)
			}
			if !matched {
				var o OrdersItem
				key := c.Name
				ks := fmt.Sprint(key)
				g, ok := groups[ks]
				if !ok {
					g = &data.Group{Key: key}
					groups[ks] = g
					order = append(order, ks)
				}
				_item := map[string]any{}
				_item["id"] = c.Id
				_item["name"] = c.Name
				_item["c"] = c
				_item["id"] = o.Id
				_item["customerId"] = o.CustomerId
				_item["o"] = o
				g.Items = append(g.Items, _item)
			}
		}
		items := []*data.Group{}
		for _, ks := range order {
			items = append(items, groups[ks])
		}
		results := []Stats{}
		for _, g := range items {
			results = append(results, Stats{
				g.Key,
				len(func() []any {
					results := []any{}
					for _, r := range g.Items {
						if _exists((r).(map[string]any)["o"]) {
							if _exists((r).(map[string]any)["o"]) {
								results = append(results, r)
							}
						}
					}
					return results
				}()),
			})
		}
		return results
	}()
	fmt.Println("--- Group Left Join ---")
	for _, s := range stats {
		fmt.Println(s.Name, "orders:", s.Count)
	}
}

func _exists(v any) bool {
	if g, ok := v.(*data.Group); ok {
		return len(g.Items) > 0
	}
	switch s := v.(type) {
	case []any:
		return len(s) > 0
	case []int:
		return len(s) > 0
	case []float64:
		return len(s) > 0
	case []string:
		return len(s) > 0
	case []bool:
		return len(s) > 0
	case []map[string]any:
		return len(s) > 0
	case map[string]any:
		return len(s) > 0
	case map[string]int:
		return len(s) > 0
	case string:
		return len([]rune(s)) > 0
	}
	rv := reflect.ValueOf(v)
	switch rv.Kind() {
	case reflect.Slice, reflect.Array:
		return rv.Len() > 0
	case reflect.Map:
		return !rv.IsNil() && rv.Len() > 0
	case reflect.Pointer:
		return !rv.IsNil()
	case reflect.Struct:
		return !rv.IsZero()
	}
	return false
}
