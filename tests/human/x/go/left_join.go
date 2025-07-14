//go:build ignore

package main

import (
	"fmt"
)

type Customer struct {
	ID   int
	Name string
}

type Order struct {
	ID         int
	CustomerID int
	Total      int
}

type Result struct {
	OrderID  int
	Customer *Customer
	Total    int
}

func main() {
	customers := []Customer{
		{ID: 1, Name: "Alice"},
		{ID: 2, Name: "Bob"},
	}

	orders := []Order{
		{ID: 100, CustomerID: 1, Total: 250},
		{ID: 101, CustomerID: 3, Total: 80},
	}

	customerMap := make(map[int]Customer)
	for _, c := range customers {
		customerMap[c.ID] = c
	}

	var result []Result
	for _, o := range orders {
		r := Result{OrderID: o.ID, Total: o.Total}
		if c, ok := customerMap[o.CustomerID]; ok {
			r.Customer = &c
		} else {
			r.Customer = nil
		}
		result = append(result, r)
	}

	fmt.Println("--- Left Join ---")
	for _, entry := range result {
		if entry.Customer != nil {
			fmt.Printf("Order %d customer {id: %d, name: %q} total %d\n",
				entry.OrderID, entry.Customer.ID, entry.Customer.Name, entry.Total)
		} else {
			fmt.Printf("Order %d customer <nil> total %d\n", entry.OrderID, entry.Total)
		}
	}
}
