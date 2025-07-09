//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
	"mochi/runtime/data"
	"reflect"
	"sort"
	"strconv"
	"strings"
)

func main() {
	type CustomersItem struct {
		Id   int    `json:"id"`
		Name string `json:"name"`
	}

	var customers []CustomersItem = []CustomersItem{
		CustomersItem{
			Id:   1,
			Name: "Alice",
		},
		CustomersItem{
			Id:   2,
			Name: "Bob",
		},
		CustomersItem{
			Id:   3,
			Name: "Charlie",
		},
		CustomersItem{
			Id:   4,
			Name: "Diana",
		},
	}
	_ = customers
	type OrdersItem struct {
		Id         int `json:"id"`
		CustomerId int `json:"customerId"`
		Total      int `json:"total"`
	}

	var orders []OrdersItem = []OrdersItem{
		OrdersItem{
			Id:         100,
			CustomerId: 1,
			Total:      250,
		},
		OrdersItem{
			Id:         101,
			CustomerId: 2,
			Total:      125,
		},
		OrdersItem{
			Id:         102,
			CustomerId: 1,
			Total:      300,
		},
		OrdersItem{
			Id:         103,
			CustomerId: 5,
			Total:      80,
		},
	}
	type Result struct {
		Order    any `json:"order"`
		Customer any `json:"customer"`
	}

	type Result1 struct {
		Order    OrdersItem    `json:"order"`
		Customer CustomersItem `json:"customer"`
	}

	var result []Result = _cast[[]Result](func() []Result1 {
		src := _toAnySlice(orders)
		resAny := _query(src, []_joinSpec{
			{items: _toAnySlice(customers), on: func(_a ...any) bool {
				o := _cast[OrdersItem](_a[0])
				_ = o
				c := _cast[CustomersItem](_a[1])
				_ = c
				return (o.CustomerId == c.Id)
			}, left: true, right: true},
		}, _queryOpts{selectFn: func(_a ...any) any {
			o := _cast[OrdersItem](_a[0])
			_ = o
			c := _cast[CustomersItem](_a[1])
			_ = c
			return Result1{
				Order:    o,
				Customer: c,
			}
		}, skip: -1, take: -1})
		out := make([]Result1, len(resAny))
		for i, v := range resAny {
			out[i] = _cast[Result1](v)
		}
		return out
	}())
	fmt.Println("--- Outer Join using syntax ---")
	for _, row := range result {
		if _exists(row.Order) {
			if _exists(row.Customer) {
				fmt.Println(strings.TrimRight(strings.Join([]string{fmt.Sprint("Order"), fmt.Sprint(_cast[map[string]any](row.Order)["id"]), fmt.Sprint("by"), fmt.Sprint(_cast[map[string]any](row.Customer)["name"]), fmt.Sprint("- $"), fmt.Sprint(_cast[map[string]any](row.Order)["total"])}, " "), " "))
			} else {
				fmt.Println(strings.TrimRight(strings.Join([]string{fmt.Sprint("Order"), fmt.Sprint(_cast[map[string]any](row.Order)["id"]), fmt.Sprint("by"), fmt.Sprint("Unknown"), fmt.Sprint("- $"), fmt.Sprint(_cast[map[string]any](row.Order)["total"])}, " "), " "))
			}
		} else {
			fmt.Println(strings.TrimRight(strings.Join([]string{fmt.Sprint("Customer"), fmt.Sprint(_cast[map[string]any](row.Customer)["name"]), fmt.Sprint("has no orders")}, " "), " "))
		}
	}
}

func _cast[T any](v any) T {
	if tv, ok := v.(T); ok {
		return tv
	}
	var out T
	switch any(out).(type) {
	case int:
		switch vv := v.(type) {
		case int:
			return any(vv).(T)
		case float64:
			return any(int(vv)).(T)
		case float32:
			return any(int(vv)).(T)
		case string:
			n, _ := strconv.Atoi(vv)
			return any(n).(T)
		}
	case float64:
		switch vv := v.(type) {
		case int:
			return any(float64(vv)).(T)
		case float64:
			return any(vv).(T)
		case float32:
			return any(float64(vv)).(T)
		}
	case float32:
		switch vv := v.(type) {
		case int:
			return any(float32(vv)).(T)
		case float64:
			return any(float32(vv)).(T)
		case float32:
			return any(vv).(T)
		}
	}
	if m, ok := v.(map[any]any); ok {
		v = _convertMapAny(m)
	}
	data, err := json.Marshal(v)
	if err != nil {
		panic(err)
	}
	if err := json.Unmarshal(data, &out); err != nil {
		panic(err)
	}
	return out
}

func _convertMapAny(m map[any]any) map[string]any {
	out := make(map[string]any, len(m))
	for k, v := range m {
		key := fmt.Sprint(k)
		if sub, ok := v.(map[any]any); ok {
			out[key] = _convertMapAny(sub)
		} else {
			out[key] = v
		}
	}
	return out
}

func _exists(v any) bool {
	if g, ok := v.(*data.Group); ok {
		return len(g.Items) > 0
	}
	switch s := v.(type) {
	case []any:
		return len(s) > 0
	case []int:
		return len(s) > 0
	case []float64:
		return len(s) > 0
	case []string:
		return len(s) > 0
	case []bool:
		return len(s) > 0
	case []map[string]any:
		return len(s) > 0
	case map[string]any:
		return len(s) > 0
	case string:
		return len([]rune(s)) > 0
	}
	rv := reflect.ValueOf(v)
	if rv.Kind() == reflect.Slice || rv.Kind() == reflect.Array {
		return rv.Len() > 0
	}
	return false
}

type _joinSpec struct {
	items []any
	on    func(...any) bool
	left  bool
	right bool
}
type _queryOpts struct {
	selectFn func(...any) any
	where    func(...any) bool
	sortKey  func(...any) any
	skip     int
	take     int
}

func _query(src []any, joins []_joinSpec, opts _queryOpts) []any {
	items := make([][]any, len(src))
	for i, v := range src {
		items[i] = []any{v}
	}
	for _, j := range joins {
		joined := [][]any{}
		if j.right && j.left {
			matched := make([]bool, len(j.items))
			for _, left := range items {
				m := false
				for ri, right := range j.items {
					keep := true
					if j.on != nil {
						args := append(append([]any(nil), left...), right)
						keep = j.on(args...)
					}
					if !keep {
						continue
					}
					m = true
					matched[ri] = true
					joined = append(joined, append(append([]any(nil), left...), right))
				}
				if !m {
					joined = append(joined, append(append([]any(nil), left...), nil))
				}
			}
			for ri, right := range j.items {
				if !matched[ri] {
					undef := make([]any, len(items[0]))
					joined = append(joined, append(undef, right))
				}
			}
		} else if j.right {
			for _, right := range j.items {
				m := false
				for _, left := range items {
					keep := true
					if j.on != nil {
						args := append(append([]any(nil), left...), right)
						keep = j.on(args...)
					}
					if !keep {
						continue
					}
					m = true
					joined = append(joined, append(append([]any(nil), left...), right))
				}
				if !m {
					undef := make([]any, len(items[0]))
					joined = append(joined, append(undef, right))
				}
			}
		} else {
			for _, left := range items {
				m := false
				for _, right := range j.items {
					keep := true
					if j.on != nil {
						args := append(append([]any(nil), left...), right)
						keep = j.on(args...)
					}
					if !keep {
						continue
					}
					m = true
					joined = append(joined, append(append([]any(nil), left...), right))
				}
				if j.left && !m {
					joined = append(joined, append(append([]any(nil), left...), nil))
				}
			}
		}
		items = joined
	}
	if opts.where != nil {
		filtered := [][]any{}
		for _, r := range items {
			if opts.where(r...) {
				filtered = append(filtered, r)
			}
		}
		items = filtered
	}
	if opts.sortKey != nil {
		type pair struct {
			item []any
			key  any
		}
		pairs := make([]pair, len(items))
		for i, it := range items {
			pairs[i] = pair{it, opts.sortKey(it...)}
		}
		sort.Slice(pairs, func(i, j int) bool {
			a, b := pairs[i].key, pairs[j].key
			switch av := a.(type) {
			case int:
				switch bv := b.(type) {
				case int:
					return av < bv
				case float64:
					return float64(av) < bv
				}
			case float64:
				switch bv := b.(type) {
				case int:
					return av < float64(bv)
				case float64:
					return av < bv
				}
			case string:
				bs, _ := b.(string)
				return av < bs
			}
			return fmt.Sprint(a) < fmt.Sprint(b)
		})
		for i, p := range pairs {
			items[i] = p.item
		}
	}
	if opts.skip >= 0 {
		if opts.skip < len(items) {
			items = items[opts.skip:]
		} else {
			items = [][]any{}
		}
	}
	if opts.take >= 0 {
		if opts.take < len(items) {
			items = items[:opts.take]
		}
	}
	res := make([]any, len(items))
	for i, r := range items {
		res[i] = opts.selectFn(r...)
	}
	return res
}

func _toAnySlice[T any](s []T) []any {
	out := []any{}
	for _, v := range s {
		out = append(out, v)
	}
	return out
}
