//go:build ignore

// Generated by Mochi v0.10.36 on 2025-07-22 18:26:26 GMT+7
package main

import (
	"fmt"
)

var people []People = []People{People{
	Name: "Alice",
	Age:  30,
}, People{
	Name: "Bob",
	Age:  15,
}, People{
	Name: "Charlie",
	Age:  65,
}, People{
	Name: "Diana",
	Age:  45,
}}

type People struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
}

type Adult struct {
	Name     string `json:"name"`
	Age      int    `json:"age"`
	IsSenior bool   `json:"is_senior"`
}

func main() {
	var adults []Adult = func() []Adult {
		res := []Adult{}
		for _, person := range people {
			if person.Age >= 18 {
				res = append(res, Adult{
					Name:     person.Name,
					Age:      person.Age,
					IsSenior: (person.Age >= 60),
				})
			}
		}
		return res
	}()
	fmt.Println("--- Adults ---")
	for _, person := range adults {
		fmt.Println(person.Name, "is", person.Age, func() string {
			if person.IsSenior {
				return " (senior)"
			} else {
				return ""
			}
		}())
	}
}
