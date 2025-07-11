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
			"Alice",
			"Paris",
		},
		PeopleItem{
			"Bob",
			"Hanoi",
		},
		PeopleItem{
			"Charlie",
			"Paris",
		},
		PeopleItem{
			"Diana",
			"Hanoi",
		},
		PeopleItem{
			"Eve",
			"Paris",
		},
		PeopleItem{
			"Frank",
			"Hanoi",
		},
		PeopleItem{
			"George",
			"Paris",
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
		results := []Big{}
		for _, ks := range order {
			g := groups[ks]
			if !(len(g.Items) >= 4) {
				continue
			}
			results = append(results, Big{
				g.Key,
				len(g.Items),
			})
		}
		return results
	}()
	func() { b, _ := json.Marshal(big); fmt.Println(string(b)) }()
}
