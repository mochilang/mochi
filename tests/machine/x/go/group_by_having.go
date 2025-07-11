//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
	"mochi/runtime/data"
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

	var big []Big = func() []Big {
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
		_res := []Big{}
		for _, ks := range order {
			g := groups[ks]
			if !(len(g.Items) >= 4) {
				continue
			}
			_res = append(_res, Big{
				City: g.Key,
				Num:  len(g.Items),
			})
		}
		return _res
	}()
	func() { b, _ := json.Marshal(big); fmt.Println(string(b)) }()
}
