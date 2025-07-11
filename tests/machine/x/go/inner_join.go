//go:build ignore

package main

import (
	"fmt"
)

func main() {
	type CustomersItem struct {
		Id   int    `json:"id"`
		Name string `json:"name"`
	}

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
	type OrdersItem struct {
		Id         int `json:"id"`
		CustomerId int `json:"customerId"`
		Total      int `json:"total"`
	}

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
	type Result struct {
		OrderId      any `json:"orderId"`
		CustomerName any `json:"customerName"`
		Total        any `json:"total"`
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
