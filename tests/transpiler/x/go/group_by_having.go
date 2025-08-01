//go:build ignore

// Generated by Mochi v0.10.36 on 2025-07-22 18:26:29 GMT+7
package main

import (
	"encoding/json"
	"fmt"
)

var people []People = []People{People{
	Name: "Alice",
	City: "Paris",
}, People{
	Name: "Bob",
	City: "Hanoi",
}, People{
	Name: "Charlie",
	City: "Paris",
}, People{
	Name: "Diana",
	City: "Hanoi",
}, People{
	Name: "Eve",
	City: "Paris",
}, People{
	Name: "Frank",
	City: "Hanoi",
}, People{
	Name: "George",
	City: "Paris",
}}

type People struct {
	Name string `json:"name"`
	City string `json:"city"`
}

type Big struct {
	City string `json:"city"`
	Num  int    `json:"num"`
}

func main() {
	var big []Big = func() []Big {
		groups := map[string]struct {
			Key   string
			Items []People
		}{}
		order := []string{}
		for _, p := range people {
			k := fmt.Sprint(p.City)
			grp, ok := groups[k]
			if !ok {
				grp = struct {
					Key   string
					Items []People
				}{Key: p.City, Items: []People{}}
				groups[k] = grp
				order = append(order, k)
			}
			grp.Items = append(grp.Items, p)
			groups[k] = grp
		}
		res := []Big{}
		for _, k := range order {
			g := groups[k]
			if len(g.Items) >= 4 {
				res = append(res, Big{
					City: g.Key,
					Num:  len(g.Items),
				})
			}
		}
		return res
	}()
	func() { b, _ := json.MarshalIndent(big, "", "  "); fmt.Println(string(b)) }()
}
