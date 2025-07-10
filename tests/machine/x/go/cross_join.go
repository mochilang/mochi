//go:build ignore

package main

import (
	"fmt"
	"strings"
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
	}, CustomersItem{
		Id:   3,
		Name: "Charlie",
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
		CustomerId: 2,
		Total:      125,
	}, OrdersItem{
		Id:         102,
		CustomerId: 1,
		Total:      300,
	}}
	type Result struct {
		OrderId            any `json:"orderId"`
		OrderCustomerId    any `json:"orderCustomerId"`
		PairedCustomerName any `json:"pairedCustomerName"`
		OrderTotal         any `json:"orderTotal"`
	}

	var result []Result = func() []Result {
		_res := []Result{}
		for _, o := range orders {
			for _, c := range customers {
				_res = append(_res, Result{
					OrderId:            o.Id,
					OrderCustomerId:    o.CustomerId,
					PairedCustomerName: c.Name,
					OrderTotal:         o.Total,
				})
			}
		}
		return _res
	}()
	fmt.Println("--- Cross Join: All order-customer pairs ---")
	for _, entry := range result {
		fmt.Println(strings.TrimRight(strings.Join([]string{fmt.Sprint("Order"), fmt.Sprint(entry.OrderId), fmt.Sprint("(customerId:"), fmt.Sprint(entry.OrderCustomerId), fmt.Sprint(", total: $"), fmt.Sprint(entry.OrderTotal), fmt.Sprint(") paired with"), fmt.Sprint(entry.PairedCustomerName)}, " "), " "))
	}
}
