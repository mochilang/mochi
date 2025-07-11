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
	}}
	_ = customers
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
	type Stats struct {
		Name  any `json:"name"`
		Count int `json:"count"`
	}

	var stats []Stats = func() []Stats {
		groups := map[string]*data.Group{}
		order := []string{}
		for _, o := range orders {
			for _, c := range customers {
				if !(o.CustomerId == c.Id) {
					continue
				}
				key := c.Name
				ks := fmt.Sprint(key)
				g, ok := groups[ks]
				if !ok {
					g = &data.Group{Key: key}
					groups[ks] = g
					order = append(order, ks)
				}
				_item := map[string]any{}
				for k, v := range o.(map[string]any) {
					_item[k] = v
				}
				_item["o"] = o
				for k, v := range c.(map[string]any) {
					_item[k] = v
				}
				_item["c"] = c
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
				Name:  g.Key,
				Count: len(g.Items),
			})
		}
		return _res
	}()
	fmt.Println(_sprint("--- Orders per customer ---"))
	for _, s := range stats {
		fmt.Println(strings.TrimRight(strings.Join([]string{_sprint(s.Name), _sprint("orders:"), _sprint(s.Count)}, " "), " "))
	}
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
