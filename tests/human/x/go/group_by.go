//go:build ignore

package main

import "fmt"

type Person struct {
	Name string
	Age  int
	City string
}

type Stat struct {
	City   string
	Count  int
	AvgAge float64
}

func main() {
	people := []Person{
		{"Alice", 30, "Paris"},
		{"Bob", 15, "Hanoi"},
		{"Charlie", 65, "Paris"},
		{"Diana", 45, "Hanoi"},
		{"Eve", 70, "Paris"},
		{"Frank", 22, "Hanoi"},
	}

	groups := make(map[string][]Person)
	for _, p := range people {
		groups[p.City] = append(groups[p.City], p)
	}

	var stats []Stat
	for city, persons := range groups {
		total := 0
		for _, p := range persons {
			total += p.Age
		}
		avg := float64(total) / float64(len(persons))
		stats = append(stats, Stat{city, len(persons), avg})
	}

	fmt.Println("--- People grouped by city ---")
	for _, s := range stats {
		fmt.Printf("%s: count = %d, avg_age = %.1f\n", s.City, s.Count, s.AvgAge)
	}
}
