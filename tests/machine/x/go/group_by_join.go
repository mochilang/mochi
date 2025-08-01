//go:build ignore

// Generated by Mochi compiler v0.10.28 on 1970-01-01T00:00:00Z

package main

import (
	"fmt"
	"mochi/runtime/data"
	"strings"
)

type v map[string]any

type Customer struct {
	ID   int    `json:"id"`
	Name string `json:"name"`
}

type Order struct {
	ID         int `json:"id"`
	CustomerID int `json:"customerId"`
}

type Stat struct {
	Name  any `json:"name"`
	Count int `json:"count"`
}

type GRow struct {
	O Order    `json:"o"`
	C Customer `json:"c"`
}

func main() {
	customers := []Customer{Customer{
		ID:   1,
		Name: "Alice",
	}, Customer{
		ID:   2,
		Name: "Bob",
	}}
	orders := []Order{Order{
		ID:         100,
		CustomerID: 1,
	}, Order{
		ID:         101,
		CustomerID: 1,
	}, Order{
		ID:         102,
		CustomerID: 2,
	}}
	stats := func() []Stat {
		groups := map[string]*data.Group{}
		order := []string{}
		for _, o := range orders {
			for _, c := range customers {
				if !(o.CustomerID == c.ID) {
					continue
				}
				key := c.Name
				ks := fmt.Sprint(key)
				g, ok := groups[ks]
				if !ok {
					g = &data.Group{Key: key}
					groups[ks] = g
					order = append(order, ks)
				}
				g.Items = append(g.Items, GRow{O: o, C: c})
			}
		}
		items := []*data.Group{}
		for _, ks := range order {
			items = append(items, groups[ks])
		}
		results := []Stat{}
		for _, g := range items {
			results = append(results, Stat{
				Name:  g.Key.(any),
				Count: len(g.Items),
			})
		}
		return results
	}()
	fmt.Println(strings.TrimSpace(fmt.Sprintln(any("--- Orders per customer ---"))))
	for _, s := range stats {
		fmt.Println(strings.TrimSpace(fmt.Sprintln(s.Name, "orders:", s.Count)))
	}
}
