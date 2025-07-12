//go:build ignore

package main

import (
	"fmt"
)

func main() {
	customers := []CustomersItem{CustomersItem{
		1,
		"Alice",
	}, CustomersItem{
		2,
		"Bob",
	}}
	_ = customers
	orders := []OrdersItem{OrdersItem{
		100,
		1,
	}, OrdersItem{
		101,
		2,
	}}
	items := []ItemsItem{ItemsItem{
		100,
		"a",
	}, ItemsItem{
		101,
		"b",
	}}
	_ = items
	result := func() []Result {
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
