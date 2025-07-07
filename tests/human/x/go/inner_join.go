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

func main() {
	customers := []Customer{{1, "Alice"}, {2, "Bob"}, {3, "Charlie"}}
	orders := []Order{{100, 1, 250}, {101, 2, 125}, {102, 1, 300}, {103, 4, 80}}

	fmt.Println("--- Orders with customer info ---")
	for _, o := range orders {
		for _, c := range customers {
			if o.CustomerID == c.ID {
				fmt.Printf("Order %d by %s - $%d\n", o.ID, c.Name, o.Total)
			}
		}
	}
}
