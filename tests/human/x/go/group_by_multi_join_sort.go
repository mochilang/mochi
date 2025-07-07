//go:build ignore

package main

import (
	"fmt"
	"sort"
)

type Nation struct {
	NationKey int
	Name      string
}

type Customer struct {
	CustKey   int
	Name      string
	AcctBal   float64
	NationKey int
	Address   string
	Phone     string
	Comment   string
}

type Order struct {
	OrderKey  int
	CustKey   int
	OrderDate string
}

type LineItem struct {
	OrderKey      int
	ReturnFlag    string
	ExtendedPrice float64
	Discount      float64
}

type Result struct {
	CustKey int
	Name    string
	Revenue float64
	AcctBal float64
	Nation  string
	Address string
	Phone   string
	Comment string
}

func main() {
	nation := []Nation{{1, "BRAZIL"}}
	customer := []Customer{{1, "Alice", 100.0, 1, "123 St", "123-456", "Loyal"}}
	orders := []Order{{1000, 1, "1993-10-15"}, {2000, 1, "1994-01-02"}}
	lineitem := []LineItem{{1000, "R", 1000.0, 0.1}, {2000, "N", 500.0, 0.0}}

	startDate := "1993-10-01"
	endDate := "1994-01-01"

	type row struct {
		c Customer
		o Order
		l LineItem
		n Nation
	}

	var rows []row
	for _, c := range customer {
		for _, o := range orders {
			if o.CustKey != c.CustKey {
				continue
			}
			if !(o.OrderDate >= startDate && o.OrderDate < endDate) {
				continue
			}
			for _, l := range lineitem {
				if l.OrderKey != o.OrderKey || l.ReturnFlag != "R" {
					continue
				}
				for _, n := range nation {
					if n.NationKey == c.NationKey {
						rows = append(rows, row{c, o, l, n})
					}
				}
			}
		}
	}

	type key struct {
		CustKey int
		Name    string
		AcctBal float64
		Address string
		Phone   string
		Comment string
		Nation  string
	}

	groups := make(map[key][]row)
	for _, r := range rows {
		k := key{r.c.CustKey, r.c.Name, r.c.AcctBal, r.c.Address, r.c.Phone, r.c.Comment, r.n.Name}
		groups[k] = append(groups[k], r)
	}

	var results []Result
	for k, g := range groups {
		revenue := 0.0
		for _, r := range g {
			revenue += r.l.ExtendedPrice * (1 - r.l.Discount)
		}
		results = append(results, Result{k.CustKey, k.Name, revenue, k.AcctBal, k.Nation, k.Address, k.Phone, k.Comment})
	}

	sort.Slice(results, func(i, j int) bool { return results[i].Revenue > results[j].Revenue })

	fmt.Println(results)
}
