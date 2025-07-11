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
			Name: "Alice",
			Age:  30,
		},
		PeopleItem{
			Name: "Bob",
			Age:  15,
		},
		PeopleItem{
			Name: "Charlie",
			Age:  65,
		},
		PeopleItem{
			Name: "Diana",
			Age:  45,
		},
	}
	type Adults struct {
		Name      any  `json:"name"`
		Age       any  `json:"age"`
		Is_senior bool `json:"is_senior"`
	}

	var adults []Adults = func() []Adults {
		_res := []Adults{}
		for _, person := range people {
			if person.Age >= 18 {
				if person.Age >= 18 {
					_res = append(_res, Adults{
						Name:      person.Name,
						Age:       person.Age,
						Is_senior: (person.Age >= 60),
					})
				}
			}
		}
		return _res
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
