//go:build ignore

// Generated by Mochi compiler v0.10.28 on 1970-01-01T00:00:00Z

package main

import (
	"fmt"
	"strings"
)

type v = Result

type Customer struct {
	ID   int    `json:"id"`
	Name string `json:"name"`
}

type Order struct {
	ID         int `json:"id"`
	CustomerID int `json:"customerId"`
	Total      int `json:"total"`
}

type Result struct {
	OrderID            int    `json:"orderId"`
	OrderCustomerID    int    `json:"orderCustomerId"`
	PairedCustomerName string `json:"pairedCustomerName"`
	OrderTotal         int    `json:"orderTotal"`
}

func main() {
	customers := []Customer{Customer{
		ID:   1,
		Name: "Alice",
	}, Customer{
		ID:   2,
		Name: "Bob",
	}, Customer{
		ID:   3,
		Name: "Charlie",
	}}
	orders := []Order{Order{
		ID:         100,
		CustomerID: 1,
		Total:      250,
	}, Order{
		ID:         101,
		CustomerID: 2,
		Total:      125,
	}, Order{
		ID:         102,
		CustomerID: 1,
		Total:      300,
	}}
	result := func() []Result {
		results := []Result{}
		for _, o := range orders {
			for _, c := range customers {
				results = append(results, Result{
					OrderID:            o.ID,
					OrderCustomerID:    o.CustomerID,
					PairedCustomerName: c.Name,
					OrderTotal:         o.Total,
				})
			}
		}
		return results
	}()
	fmt.Println(strings.TrimSpace(fmt.Sprintln(any("--- Cross Join: All order-customer pairs ---"))))
	for _, entry := range result {
		fmt.Println(strings.TrimSpace(fmt.Sprintln(any("Order"), entry.OrderID, "(customerId:", entry.OrderCustomerID, ", total: $", entry.OrderTotal, ") paired with", entry.PairedCustomerName)))
	}
}
