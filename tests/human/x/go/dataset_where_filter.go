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
	var adults []map[string]any = func() []map[string]any {
		_res := []map[string]any{}
		for _, person := range people {
			if person.Age >= 18 {
				if person.Age >= 18 {
					_res = append(_res, map[string]any{
						"name":      person.Name,
						"age":       person.Age,
						"is_senior": (person.Age >= 60),
					})
				}
			}
		}
		return _res
	}()
	fmt.Println("--- Adults ---")
	for _, person := range adults {
		fmt.Println(person["name"], "is", person["age"], func() string {
			if person["is_senior"] {
				return " (senior)"
			} else {
				return ""
			}
		}())
	}
}
