//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
	"mochi/runtime/data"
)

func main() {
	var people []map[string]string = []map[string]string{
		map[string]string{"name": "Alice", "city": "Paris"},
		map[string]string{"name": "Bob", "city": "Hanoi"},
		map[string]string{"name": "Charlie", "city": "Paris"},
		map[string]string{"name": "Diana", "city": "Hanoi"},
		map[string]string{"name": "Eve", "city": "Paris"},
		map[string]string{"name": "Frank", "city": "Hanoi"},
		map[string]string{"name": "George", "city": "Paris"},
	}
	var big []map[string]any = func() []map[string]any {
		groups := map[string]*data.Group{}
		order := []string{}
		for _, p := range people {
			key := p["city"]
			ks := fmt.Sprint(key)
			g, ok := groups[ks]
			if !ok {
				g = &data.Group{Key: key}
				groups[ks] = g
				order = append(order, ks)
			}
			g.Items = append(g.Items, p)
		}
		_res := []map[string]any{}
		for _, ks := range order {
			g := groups[ks]
			if !(len(g.Items) >= 4) {
				continue
			}
			_res = append(_res, map[string]any{"city": g.Key, "num": len(g.Items)})
		}
		return _res
	}()
	func() { b, _ := json.Marshal(big); fmt.Println(string(b)) }()
}
