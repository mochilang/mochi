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
		CustomerId: 2,
	}}
	type ItemsItem struct {
		OrderId int    `json:"orderId"`
		Sku     string `json:"sku"`
	}

	var items []ItemsItem = []ItemsItem{ItemsItem{
		OrderId: 100,
		Sku:     "a",
	}, ItemsItem{
		OrderId: 101,
		Sku:     "b",
	}}
	_ = items
	type Result struct {
		Name any `json:"name"`
		Sku  any `json:"sku"`
	}

	var result []Result = func() []Result {
		_res := []Result{}
		for _, o := range orders {
			for _, c := range customers {
				if !(o.CustomerId == c.Id) {
					continue
				}
				for _, i := range items {
					if !(o.Id == i.OrderId) {
						continue
					}
					_res = append(_res, Result{
						Name: c.Name,
						Sku:  i.Sku,
					})
				}
			}
		}
		return _res
	}()
	fmt.Println("--- Multi Join ---")
	for _, r := range result {
		fmt.Println(r.Name, "bought item", r.Sku)
	}
}
