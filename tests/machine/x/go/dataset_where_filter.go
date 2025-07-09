//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
	"strconv"
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

	type Result struct {
		Name      string `json:"name"`
		Age       int    `json:"age"`
		Is_senior bool   `json:"is_senior"`
	}

	var adults []Adults = _cast[[]Adults](func() []Result {
		_res := []Result{}
		for _, person := range people {
			if person.Age >= 18 {
				if person.Age >= 18 {
					_res = append(_res, Result{
						Name:      person.Name,
						Age:       person.Age,
						Is_senior: (person.Age >= 60),
					})
				}
			}
		}
		return _res
	}())
	fmt.Println("--- Adults ---")
	for _, person := range adults {
		fmt.Println(strings.TrimRight(strings.Join([]string{fmt.Sprint(person.Name), fmt.Sprint("is"), fmt.Sprint(person.Age), fmt.Sprint(func() string {
			if person.Is_senior {
				return " (senior)"
			} else {
				return ""
			}
		}())}, " "), " "))
	}
}

func _cast[T any](v any) T {
	if tv, ok := v.(T); ok {
		return tv
	}
	var out T
	switch any(out).(type) {
	case int:
		switch vv := v.(type) {
		case int:
			return any(vv).(T)
		case float64:
			return any(int(vv)).(T)
		case float32:
			return any(int(vv)).(T)
		case string:
			n, _ := strconv.Atoi(vv)
			return any(n).(T)
		}
	case float64:
		switch vv := v.(type) {
		case int:
			return any(float64(vv)).(T)
		case float64:
			return any(vv).(T)
		case float32:
			return any(float64(vv)).(T)
		}
	case float32:
		switch vv := v.(type) {
		case int:
			return any(float32(vv)).(T)
		case float64:
			return any(float32(vv)).(T)
		case float32:
			return any(vv).(T)
		}
	}
	if m, ok := v.(map[any]any); ok {
		v = _convertMapAny(m)
	}
	data, err := json.Marshal(v)
	if err != nil {
		panic(err)
	}
	if err := json.Unmarshal(data, &out); err != nil {
		panic(err)
	}
	return out
}

func _convertMapAny(m map[any]any) map[string]any {
	out := make(map[string]any, len(m))
	for k, v := range m {
		key := fmt.Sprint(k)
		if sub, ok := v.(map[any]any); ok {
			out[key] = _convertMapAny(sub)
		} else {
			out[key] = v
		}
	}
	return out
}
