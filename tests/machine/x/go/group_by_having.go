//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
	"mochi/runtime/data"
	"strconv"
)

func main() {
	type PeopleItem struct {
		Name string `json:"name"`
		City string `json:"city"`
	}

	var people []PeopleItem = []PeopleItem{
		PeopleItem{
			Name: "Alice",
			City: "Paris",
		},
		PeopleItem{
			Name: "Bob",
			City: "Hanoi",
		},
		PeopleItem{
			Name: "Charlie",
			City: "Paris",
		},
		PeopleItem{
			Name: "Diana",
			City: "Hanoi",
		},
		PeopleItem{
			Name: "Eve",
			City: "Paris",
		},
		PeopleItem{
			Name: "Frank",
			City: "Hanoi",
		},
		PeopleItem{
			Name: "George",
			City: "Paris",
		},
	}
	type Big struct {
		City any `json:"city"`
		Num  int `json:"num"`
	}

	type Result struct {
		City any `json:"city"`
		Num  int `json:"num"`
	}

	var big []Big = _cast[[]Big](func() []Result {
		groups := map[string]*data.Group{}
		order := []string{}
		for _, p := range people {
			key := p.City
			ks := fmt.Sprint(key)
			g, ok := groups[ks]
			if !ok {
				g = &data.Group{Key: key}
				groups[ks] = g
				order = append(order, ks)
			}
			g.Items = append(g.Items, p)
		}
		_res := []Result{}
		for _, ks := range order {
			g := groups[ks]
			if !(len(g.Items) >= 4) {
				continue
			}
			_res = append(_res, Result{
				City: g.Key,
				Num:  len(g.Items),
			})
		}
		return _res
	}())
	func() { b, _ := json.Marshal(big); fmt.Println(string(b)) }()
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
