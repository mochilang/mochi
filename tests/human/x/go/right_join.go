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
	CustomerName string
	Order        *Order
}

func main() {
	customers := []Customer{{1, "Alice"}, {2, "Bob"}, {3, "Charlie"}, {4, "Diana"}}
	orders := []Order{{100, 1, 250}, {101, 2, 125}, {102, 1, 300}}

	orderByCust := make(map[int][]Order)
	for _, o := range orders {
		orderByCust[o.CustomerID] = append(orderByCust[o.CustomerID], o)
	}

	var result []Row
	for _, c := range customers {
		if list, ok := orderByCust[c.ID]; ok {
			for _, o := range list {
				result = append(result, Row{c.Name, &o})
			}
		} else {
			result = append(result, Row{c.Name, nil})
		}
	}

	fmt.Println("--- Right Join using syntax ---")
	for _, r := range result {
		if r.Order != nil {
			fmt.Printf("Customer %s has order %d - $%d\n", r.CustomerName, r.Order.ID, r.Order.Total)
		} else {
			fmt.Printf("Customer %s has no orders\n", r.CustomerName)
		}
	}
}
