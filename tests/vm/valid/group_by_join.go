package main

import (
	"fmt"
	"mochi/runtime/data"
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
	var stats []map[string]any = func() []map[string]any {
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
				for k, v := range _cast[map[string]any](o) {
					_item[k] = v
				}
				_item["o"] = o
				for k, v := range _cast[map[string]any](c) {
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
		_res := []map[string]any{}
		for _, g := range items {
			_res = append(_res, map[string]any{"name": g.Key, "count": len(g.Items)})
		}
		return _res
	}()
	fmt.Println("--- Orders per customer ---")
	for _, s := range stats {
		fmt.Println(s["name"], "orders:", s["count"])
	}
}
