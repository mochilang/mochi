//go:build ignore

// Generated by Mochi v0.10.36 on 2025-07-22 18:26:30 GMT+7
package main

import (
	"encoding/json"
	"fmt"
	"sort"
	"strings"
)

var items []Item = []Item{Item{
	A:   "x",
	B:   1,
	Val: 2,
}, Item{
	A:   "x",
	B:   2,
	Val: 3,
}, Item{
	A:   "y",
	B:   1,
	Val: 4,
}, Item{
	A:   "y",
	B:   2,
	Val: 1,
}}

type Item struct {
	A   string `json:"a"`
	B   int    `json:"b"`
	Val int    `json:"val"`
}

type GroupedKey struct {
	A string `json:"a"`
	B int    `json:"b"`
}

type Grouped struct {
	A     string `json:"a"`
	B     int    `json:"b"`
	Total int    `json:"total"`
}

func main() {
	var grouped []Grouped = func() []Grouped {
		groups := map[string]struct {
			Key   GroupedKey
			Items []Item
		}{}
		order := []string{}
		for _, i := range items {
			k := fmt.Sprint(GroupedKey{
				A: i.A,
				B: i.B,
			})
			grp, ok := groups[k]
			if !ok {
				grp = struct {
					Key   GroupedKey
					Items []Item
				}{Key: GroupedKey{
					A: i.A,
					B: i.B,
				}, Items: []Item{}}
				groups[k] = grp
				order = append(order, k)
			}
			grp.Items = append(grp.Items, i)
			groups[k] = grp
		}
		type pair struct {
			Key int
			Val Grouped
		}
		pairs := []pair{}
		for _, k := range order {
			g := groups[k]
			pairs = append(pairs, pair{(0 - func() int {
				s := 0
				for _, n := range func() []int {
					res := []int{}
					for _, x := range g.Items {
						res = append(res, x.Val)
					}
					return res
				}() {
					s += n
				}
				return s
			}()), Grouped{
				A: g.Key.(map[string]any)["a"],
				B: g.Key.(map[string]any)["b"],
				Total: func() int {
					s := 0
					for _, n := range func() []int {
						res := []int{}
						for _, x := range g.Items {
							res = append(res, x.Val)
						}
						return res
					}() {
						s += n
					}
					return s
				}(),
			}})
		}
		sort.Slice(pairs, func(i, j int) bool { return pairs[i].Key < pairs[j].Key })
		res := make([]Grouped, len(pairs))
		for i, p := range pairs {
			res[i] = p.Val
		}
		return res
	}()
	fmt.Println(func() string {
		b, _ := json.Marshal(grouped)
		s := string(b)
		s = strings.ReplaceAll(s, ":", ": ")
		s = strings.ReplaceAll(s, ",", ", ")
		s = strings.ReplaceAll(s, "}, {", "},{")
		s = strings.ReplaceAll(s, "\"", "'")
		return s
	}())
}
