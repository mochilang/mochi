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
	}}
	_ = customers
	type OrdersItem struct {
		Id         int `json:"id"`
		CustomerId int `json:"customerId"`
	}

	var orders []OrdersItem = []OrdersItem{OrdersItem{
		100,
		1,
	}, OrdersItem{
		101,
		2,
	}}
	type ItemsItem struct {
		OrderId int    `json:"orderId"`
		Sku     string `json:"sku"`
	}

	var items []ItemsItem = []ItemsItem{ItemsItem{
		100,
		"a",
	}, ItemsItem{
		101,
		"b",
	}}
	_ = items
	type Result struct {
		Name any `json:"name"`
		Sku  any `json:"sku"`
	}

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
