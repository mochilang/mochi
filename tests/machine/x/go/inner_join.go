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
	}, CustomersItem{
		3,
		"Charlie",
	}}
	_ = customers
	var orders []OrdersItem = []OrdersItem{
		OrdersItem{
			100,
			1,
			250,
		},
		OrdersItem{
			101,
			2,
			125,
		},
		OrdersItem{
			102,
			1,
			300,
		},
		OrdersItem{
			103,
			4,
			80,
		},
	}
	var result []Result = func() []Result {
		results := []Result{}
		for _, o := range orders {
			for _, c := range customers {
				if !(o.CustomerId == c.Id) {
					continue
				}
				results = append(results, Result{
					o.Id,
					c.Name,
					o.Total,
				})
			}
		}
		return results
	}()
	fmt.Println("--- Orders with customer info ---")
	for _, entry := range result {
		fmt.Println("Order", entry.OrderId, "by", entry.CustomerName, "- $", entry.Total)
	}
}
