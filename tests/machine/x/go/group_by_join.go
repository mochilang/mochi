//go:build ignore

package main

import (
	"fmt"
	"mochi/runtime/data"
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
		fmt.Println(s.Name, "orders:", s.Count)
	}
}
