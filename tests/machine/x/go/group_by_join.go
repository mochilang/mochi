//go:build ignore

package main

import (
	"fmt"
	"mochi/runtime/data"
	"reflect"
)

func main() {
	var customers []CustomersItem = []CustomersItem{CustomersItem{
		1,
		"Alice",
	}, CustomersItem{
		2,
		"Bob",
	}}
	_ = customers
	var orders []OrdersItem = []OrdersItem{OrdersItem{
		100,
		1,
	}, OrdersItem{
		101,
		1,
	}, OrdersItem{
		102,
		2,
	}}
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
				_item["id"] = o.Id
				_item["customerId"] = o.CustomerId
				_item["o"] = o
				_item["id"] = c.Id
				_item["name"] = c.Name
				_item["c"] = c
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
				len(g.Items),
			})
		}
		return results
	}()
	fmt.Println("--- Orders per customer ---")
	for _, s := range stats {
		_print(s.Name, "orders:", s.Count)
	}
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
