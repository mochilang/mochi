//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

func main() {
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
		_print(person.Name, "is", person.Age, func() string {
			if person.Is_senior {
				return " (senior)"
			} else {
				return ""
			}
		}())
	}
}

func _print(args ...any) {
	first := true
	for _, a := range args {
		if !first {
			fmt.Print(" ")
		}
		first = false
		rv := reflect.ValueOf(a)
		if a == nil || ((rv.Kind() == reflect.Map || rv.Kind() == reflect.Slice) && rv.IsNil()) {
			fmt.Print("<nil>")
			continue
		}
		if rv.Kind() == reflect.Slice && rv.Type().Elem().Kind() != reflect.Uint8 {
			for i := 0; i < rv.Len(); i++ {
				if i > 0 {
					fmt.Print(" ")
				}
				fmt.Print(_sprint(rv.Index(i).Interface()))
			}
			continue
		}
		fmt.Print(_sprint(a))
	}
	fmt.Println()
}

func _sprint(v any) string {
	if v == nil {
		return "<nil>"
	}
	rv := reflect.ValueOf(v)
	if (rv.Kind() == reflect.Map || rv.Kind() == reflect.Slice) && rv.IsNil() {
		return "<nil>"
	}
	return fmt.Sprint(v)
}
