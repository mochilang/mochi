//go:build ignore

package main

import (
	"fmt"
)

func main() {
	people := []PeopleItem{
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
	adults := func() []Adults {
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
