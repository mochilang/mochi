//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
	"reflect"
	"strings"
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
	fmt.Println(_fmt("--- Adults ---"))
	for _, person := range adults {
		fmt.Println(strings.TrimRight(strings.Join([]string{_fmt(person.Name), _fmt("is"), _fmt(person.Age), _fmt(func() string {
			if person.Is_senior {
				return " (senior)"
			} else {
				return ""
			}
		}())}, " "), " "))
	}
}

func _fmt(v any) string {
	if v == nil {
		return "<nil>"
	}
	rv := reflect.ValueOf(v)
	if rv.Kind() == reflect.Pointer {
		if rv.IsNil() {
			return "<nil>"
		}
		v = rv.Elem().Interface()
		rv = reflect.ValueOf(v)
	}
	if rv.Kind() == reflect.Struct {
		if rv.IsZero() {
			return "<nil>"
		}
		b, _ := json.Marshal(v)
		var m map[string]any
		_ = json.Unmarshal(b, &m)
		return fmt.Sprint(m)
	}
	return fmt.Sprint(v)
}
