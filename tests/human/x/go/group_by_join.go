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
	customers := []Customer{{1, "Alice"}, {2, "Bob"}}
	orders := []Order{{100, 1}, {101, 1}, {102, 2}}

	counts := make(map[string]int)
	for _, o := range orders {
		for _, c := range customers {
			if o.CustomerID == c.ID {
				counts[c.Name]++
			}
		}
	}

	var stats []Stat
	for name, count := range counts {
		stats = append(stats, Stat{name, count})
	}

	fmt.Println("--- Orders per customer ---")
	for _, s := range stats {
		fmt.Printf("%s orders: %d\n", s.Name, s.Count)
	}
}
