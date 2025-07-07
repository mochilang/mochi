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

type Stat struct {
	Name  string
	Count int
}

func main() {
	customers := []Customer{{1, "Alice"}, {2, "Bob"}, {3, "Charlie"}}
	orders := []Order{{100, 1}, {101, 1}, {102, 2}}

	stats := make([]Stat, 0, len(customers))
	for _, c := range customers {
		count := 0
		for _, o := range orders {
			if o.CustomerID == c.ID {
				count++
			}
		}
		stats = append(stats, Stat{c.Name, count})
	}

	fmt.Println("--- Group Left Join ---")
	for _, s := range stats {
		fmt.Printf("%s orders: %d\n", s.Name, s.Count)
	}
}
