//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
)

type Person struct {
	Name string `json:"name"`
	City string `json:"city"`
}

type Result struct {
	City string `json:"city"`
	Num  int    `json:"num"`
}

func main() {
	people := []Person{
		{Name: "Alice", City: "Paris"},
		{Name: "Bob", City: "Hanoi"},
		{Name: "Charlie", City: "Paris"},
		{Name: "Diana", City: "Hanoi"},
		{Name: "Eve", City: "Paris"},
		{Name: "Frank", City: "Hanoi"},
		{Name: "George", City: "Paris"},
	}

	counts := make(map[string]int)
	for _, p := range people {
		counts[p.City]++
	}

	var results []Result
	for city, num := range counts {
		if num >= 4 {
			results = append(results, Result{City: city, Num: num})
		}
	}

	data, _ := json.Marshal(results)
	fmt.Println(string(data))
}
