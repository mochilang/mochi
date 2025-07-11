//go:build ignore

package main

import (
	"fmt"
	"mochi/runtime/data"
	"reflect"
	"strings"
)

func main() {
	type NationsItem struct {
		Id   int    `json:"id"`
		Name string `json:"name"`
	}

	var nations []NationsItem = []NationsItem{NationsItem{
		Id:   1,
		Name: "A",
	}, NationsItem{
		Id:   2,
		Name: "B",
	}}
	_ = nations
	type SuppliersItem struct {
		Id     int `json:"id"`
		Nation int `json:"nation"`
	}

	var suppliers []SuppliersItem = []SuppliersItem{SuppliersItem{
		Id:     1,
		Nation: 1,
	}, SuppliersItem{
		Id:     2,
		Nation: 2,
	}}
	_ = suppliers
	type PartsuppItem struct {
		Part     int     `json:"part"`
		Supplier int     `json:"supplier"`
		Cost     float64 `json:"cost"`
		Qty      int     `json:"qty"`
	}

	var partsupp []PartsuppItem = []PartsuppItem{PartsuppItem{
		Part:     100,
		Supplier: 1,
		Cost:     10.0,
		Qty:      2,
	}, PartsuppItem{
		Part:     100,
		Supplier: 2,
		Cost:     20.0,
		Qty:      1,
	}, PartsuppItem{
		Part:     200,
		Supplier: 1,
		Cost:     5.0,
		Qty:      3,
	}}
	type Filtered struct {
		Part  any `json:"part"`
		Value any `json:"value"`
	}

	var filtered []Filtered = func() []Filtered {
		_res := []Filtered{}
		for _, ps := range partsupp {
			for _, s := range suppliers {
				if !(s.Id == ps.Supplier) {
					continue
				}
				for _, n := range nations {
					if !(n.Id == s.Nation) {
						continue
					}
					if n.Name == "A" {
						if n.Name == "A" {
							_res = append(_res, Filtered{
								Part:  ps.Part,
								Value: (ps.Cost * float64(ps.Qty)),
							})
						}
					}
				}
			}
		}
		return _res
	}()
	type Grouped struct {
		Part  any `json:"part"`
		Total int `json:"total"`
	}

	var grouped []Grouped = func() []Grouped {
		groups := map[string]*data.Group{}
		order := []string{}
		for _, x := range filtered {
			key := x.Part
			ks := fmt.Sprint(key)
			g, ok := groups[ks]
			if !ok {
				g = &data.Group{Key: key}
				groups[ks] = g
				order = append(order, ks)
			}
			g.Items = append(g.Items, x)
		}
		_res := []Grouped{}
		for _, ks := range order {
			g := groups[ks]
			_res = append(_res, Grouped{
				Part: g.Key,
				Total: _sum(func() []any {
					_res := []any{}
					for _, r := range g.Items {
						_res = append(_res, (r).(map[string]any)["value"])
					}
					return _res
				}()),
			})
		}
		return _res
	}()
	fmt.Println(strings.TrimSuffix(strings.TrimPrefix(_sprint(grouped), "["), "]"))
}

func _sprint(v any) string {
	if v == nil {
		return "<nil>"
	}
	rv := reflect.ValueOf(v)
	if (rv.Kind() == reflect.Map || rv.Kind() == reflect.Slice) && rv.IsNil() {
		return "<nil>"
	}
	return fmt.Sprint(v)
}

func _sum(v any) float64 {
	var items []any
	if g, ok := v.(*data.Group); ok {
		items = g.Items
	} else {
		switch s := v.(type) {
		case []any:
			items = s
		case []int:
			items = []any{}
			for _, v := range s {
				items = append(items, v)
			}
		case []float64:
			items = []any{}
			for _, v := range s {
				items = append(items, v)
			}
		case []string, []bool:
			panic("sum() expects numbers")
		default:
			panic("sum() expects list or group")
		}
	}
	var sum float64
	for _, it := range items {
		switch n := it.(type) {
		case int:
			sum += float64(n)
		case int64:
			sum += float64(n)
		case float64:
			sum += n
		default:
			panic("sum() expects numbers")
		}
	}
	return sum
}
