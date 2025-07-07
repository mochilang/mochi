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
	customers := []Customer{{1, "Alice"}, {2, "Bob"}}
	orders := []Order{{100, 1, 250}, {101, 3, 80}}

	fmt.Println("--- Left Join ---")
	for _, o := range orders {
		var cName string
		for _, c := range customers {
			if c.ID == o.CustomerID {
				cName = c.Name
				break
			}
		}
		fmt.Printf("Order %d customer %s total %d\n", o.ID, cName, o.Total)
	}
}
