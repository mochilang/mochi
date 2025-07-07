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
	items := []Item{{100, "a"}}

	fmt.Println("--- Left Join Multi ---")
	for _, o := range orders {
		var cName string
		for _, c := range customers {
			if c.ID == o.CustomerID {
				cName = c.Name
				break
			}
		}
		var item *Item
		for _, it := range items {
			if it.OrderID == o.ID {
				tmp := it
				item = &tmp
			}
		}
		fmt.Println(o.ID, cName, item)
	}
}
