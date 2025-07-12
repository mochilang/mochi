//go:build ignore

package main

import (
	"fmt"
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
		2,
	}}
	var items []ItemsItem = []ItemsItem{ItemsItem{
		100,
		"a",
	}, ItemsItem{
		101,
		"b",
	}}
	_ = items
	var result []Result = func() []Result {
		results := []Result{}
		for _, o := range orders {
			for _, c := range customers {
				if !(o.CustomerId == c.Id) {
					continue
				}
				for _, i := range items {
					if !(o.Id == i.OrderId) {
						continue
					}
					results = append(results, Result{
						c.Name,
						i.Sku,
					})
				}
			}
		}
		return results
	}()
	fmt.Println("--- Multi Join ---")
	for _, r := range result {
		fmt.Println(r.Name, "bought item", r.Sku)
	}
}
