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
}

type Item struct {
	OrderID int
	SKU     string
}

func main() {
	customers := []Customer{{1, "Alice"}, {2, "Bob"}}
	orders := []Order{{100, 1}, {101, 2}}
	items := []Item{{100, "a"}, {101, "b"}}

	fmt.Println("--- Multi Join ---")
	for _, o := range orders {
		for _, c := range customers {
			if c.ID != o.CustomerID {
				continue
			}
			for _, i := range items {
				if i.OrderID == o.ID {
					fmt.Printf("%s bought item %s\n", c.Name, i.SKU)
				}
			}
		}
	}
}
