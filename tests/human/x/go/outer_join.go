//go:build ignore

package main

import "fmt"

type Customer struct {
	ID   int
	Name string
}

type Order struct {
	ID         int
	CustomerID int
	Total      int
}

type Row struct {
	Order    *Order
	Customer *Customer
}

func main() {
	customers := []Customer{{1, "Alice"}, {2, "Bob"}, {3, "Charlie"}, {4, "Diana"}}
	orders := []Order{{100, 1, 250}, {101, 2, 125}, {102, 1, 300}, {103, 5, 80}}

	customerByID := make(map[int]Customer)
	for _, c := range customers {
		customerByID[c.ID] = c
	}

	seenCustomer := make(map[int]bool)
	var result []Row
	for _, o := range orders {
		c, ok := customerByID[o.CustomerID]
		if ok {
			seenCustomer[c.ID] = true
			result = append(result, Row{&o, &c})
		} else {
			result = append(result, Row{&o, nil})
		}
	}

	for _, c := range customers {
		if !seenCustomer[c.ID] {
			result = append(result, Row{nil, &c})
		}
	}

	fmt.Println("--- Outer Join using syntax ---")
	for _, row := range result {
		if row.Order != nil {
			if row.Customer != nil {
				fmt.Printf("Order %d by %s - $%d\n", row.Order.ID, row.Customer.Name, row.Order.Total)
			} else {
				fmt.Printf("Order %d by Unknown - $%d\n", row.Order.ID, row.Order.Total)
			}
		} else if row.Customer != nil {
			fmt.Printf("Customer %s has no orders\n", row.Customer.Name)
		}
	}
}
