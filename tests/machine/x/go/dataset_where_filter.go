//go:build ignore

package main

import (
	"fmt"
)

func main() {
	type PeopleItem struct {
		Name string `json:"name"`
		Age  int    `json:"age"`
	}

	var people []PeopleItem = []PeopleItem{
		PeopleItem{
			"Alice",
			30,
		},
		PeopleItem{
			"Bob",
			15,
		},
		PeopleItem{
			"Charlie",
			65,
		},
		PeopleItem{
			"Diana",
			45,
		},
	}
	type Adults struct {
		Name      any  `json:"name"`
		Age       any  `json:"age"`
		Is_senior bool `json:"is_senior"`
	}

	var adults []Adults = func() []Adults {
		results := []Adults{}
		for _, person := range people {
			if person.Age >= 18 {
				if person.Age >= 18 {
					results = append(results, Adults{
						person.Name,
						person.Age,
						(person.Age >= 60),
					})
				}
			}
		}
		return results
	}()
	fmt.Println("--- Adults ---")
	for _, person := range adults {
		fmt.Println(person.Name, "is", person.Age, func() string {
			if person.Is_senior {
				return " (senior)"
			} else {
				return ""
			}
		}())
	}
}
