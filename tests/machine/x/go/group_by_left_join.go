//go:build ignore

package main

import (
	"fmt"
	"mochi/runtime/data"
	"reflect"
	"strings"
)

func main() {
	type CustomersItem struct {
		Id   int    `json:"id"`
		Name string `json:"name"`
	}

	var customers []CustomersItem = []CustomersItem{CustomersItem{
		Id:   1,
		Name: "Alice",
	}, CustomersItem{
		Id:   2,
		Name: "Bob",
	}, CustomersItem{
		Id:   3,
		Name: "Charlie",
	}}
	type OrdersItem struct {
		Id         int `json:"id"`
		CustomerId int `json:"customerId"`
	}

	var orders []OrdersItem = []OrdersItem{OrdersItem{
		Id:         100,
		CustomerId: 1,
	}, OrdersItem{
		Id:         101,
		CustomerId: 1,
	}, OrdersItem{
		Id:         102,
		CustomerId: 2,
	}}
	_ = orders
	type Stats struct {
		Name  any `json:"name"`
		Count int `json:"count"`
	}

	var stats []Stats = func() []Stats {
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
				for k, v := range c.(map[string]any) {
					_item[k] = v
				}
				_item["c"] = c
				for k, v := range o.(map[string]any) {
					_item[k] = v
				}
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
				for k, v := range c.(map[string]any) {
					_item[k] = v
				}
				_item["c"] = c
				for k, v := range o.(map[string]any) {
					_item[k] = v
				}
				_item["o"] = o
				g.Items = append(g.Items, _item)
			}
		}
		items := []*data.Group{}
		for _, ks := range order {
			items = append(items, groups[ks])
		}
		_res := []Stats{}
		for _, g := range items {
			_res = append(_res, Stats{
				Name: g.Key,
				Count: len(func() []any {
					_res := []any{}
					for _, r := range g.Items {
						if _exists((r).(map[string]any)["o"]) {
							if _exists((r).(map[string]any)["o"]) {
								_res = append(_res, r)
							}
						}
					}
					return _res
				}()),
			})
		}
		return _res
	}()
	fmt.Println(_sprint("--- Group Left Join ---"))
	for _, s := range stats {
		fmt.Println(strings.TrimRight(strings.Join([]string{_sprint(s.Name), _sprint("orders:"), _sprint(s.Count)}, " "), " "))
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
		return true
	}
	return false
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
