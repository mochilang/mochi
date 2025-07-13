//go:build ignore

package main

import (
    "fmt"
    "mochi/runtime/data"
)

type PeopleItem struct {
    Name string `json:"name"`
    Age  int    `json:"age"`
    City string `json:"city"`
}

type Stats struct {
    City   string `json:"city"`
    Count  int    `json:"count"`
    AvgAge float64 `json:"avg_age"`
}

func main() {
    people := []PeopleItem{{"Alice", 30, "Paris"}, {"Bob", 15, "Hanoi"}, {"Charlie", 65, "Paris"}, {"Diana", 45, "Hanoi"}, {"Eve", 70, "Paris"}, {"Frank", 22, "Hanoi"}}
    stats := func() []Stats {
        groups := map[string]*data.Group{}
        order := []string{}
        for _, person := range people {
            key := person.City
            ks := fmt.Sprint(key)
            g, ok := groups[ks]
            if !ok {
                g = &data.Group{Key: key}
                groups[ks] = g
                order = append(order, ks)
            }
            g.Items = append(g.Items, person)
        }
        results := []Stats{}
        for _, ks := range order {
            g := groups[ks]
            results = append(results, Stats{
                g.Key,
                len(g.Items),
                _avg(func() []any {
                    results := []any{}
                    for _, p := range g.Items {
                        results = append(results, _toAnyMap(p)["age"])
                    }
                    return results
                }()),
            })
        }
        return results
    }()
    fmt.Println("--- People grouped by city ---")
    for _, s := range stats {
        fmt.Println(s.City, ": count =", s.Count, ", avg_age =", s.AvgAge)
    }
}
