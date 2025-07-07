//go:build ignore

package main

import "fmt"

type Nation struct {
	ID   int
	Name string
}

type Supplier struct {
	ID     int
	Nation int
}

type PartSupp struct {
	Part     int
	Supplier int
	Cost     float64
	Qty      int
}

type Record struct {
	Part  int
	Value float64
}

type Result struct {
	Part  int
	Total float64
}

func main() {
	nations := []Nation{{1, "A"}, {2, "B"}}
	suppliers := []Supplier{{1, 1}, {2, 2}}
	partsupp := []PartSupp{{100, 1, 10.0, 2}, {100, 2, 20.0, 1}, {200, 1, 5.0, 3}}

	var filtered []Record
	for _, ps := range partsupp {
		var sup Supplier
		ok := false
		for _, s := range suppliers {
			if s.ID == ps.Supplier {
				sup = s
				ok = true
				break
			}
		}
		if !ok {
			continue
		}
		for _, n := range nations {
			if n.ID == sup.Nation && n.Name == "A" {
				filtered = append(filtered, Record{ps.Part, ps.Cost * float64(ps.Qty)})
			}
		}
	}

	totals := make(map[int]float64)
	for _, r := range filtered {
		totals[r.Part] += r.Value
	}

	var results []Result
	for part, total := range totals {
		results = append(results, Result{part, total})
	}

	fmt.Println(results)
}
