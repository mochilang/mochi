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
		Total      int `json:"total"`
	}

	var orders []OrdersItem = []OrdersItem{OrdersItem{
		Id:         100,
		CustomerId: 1,
		Total:      250,
	}, OrdersItem{
		Id:         101,
		CustomerId: 3,
		Total:      80,
	}}
	var result []map[string]any = func() []map[string]any {
		_res := []map[string]any{}
		for _, o := range orders {
			matched := false
			for _, c := range customers {
				if !(o.CustomerId == c.Id) {
					continue
				}
				matched = true
				_res = append(_res, map[string]any{
					"orderId":  o.Id,
					"customer": c,
					"total":    o.Total,
				})
			}
			if !matched {
				var c CustomersItem
				_res = append(_res, map[string]any{
					"orderId":  o.Id,
					"customer": c,
					"total":    o.Total,
				})
			}
		}
		return _res
	}()
	fmt.Println("--- Left Join ---")
	for _, entry := range result {
		fmt.Println("Order", entry["orderId"], "customer", entry["customer"], "total", entry["total"])
	}
}
