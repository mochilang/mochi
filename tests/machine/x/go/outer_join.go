//go:build ignore

package main

import (
	"fmt"
	"mochi/runtime/data"
	"reflect"
	"sort"
	"strings"
)

func main() {
	var customers []CustomersItem = []CustomersItem{
		CustomersItem{
			1,
			"Alice",
		},
		CustomersItem{
			2,
			"Bob",
		},
		CustomersItem{
			3,
			"Charlie",
		},
		CustomersItem{
			4,
			"Diana",
		},
	}
	_ = customers
	var orders []OrdersItem = []OrdersItem{
		OrdersItem{
			100,
			1,
			250,
		},
		OrdersItem{
			101,
			2,
			125,
		},
		OrdersItem{
			102,
			1,
			300,
		},
		OrdersItem{
			103,
			5,
			80,
		},
	}
	var result []Result = func() []Result {
		src := _toAnySlice(orders)
		resAny := _query(src, []_joinSpec{
			{items: _toAnySlice(customers), on: func(_a ...any) bool {
				tmp0 := _a[0]
				var o OrdersItem
				if tmp0 != nil {
					o = tmp0.(OrdersItem)
				}
				_ = o
				tmp1 := _a[1]
				var c CustomersItem
				if tmp1 != nil {
					c = tmp1.(CustomersItem)
				}
				_ = c
				return (o.CustomerId == c.Id)
			}, leftKey: func(_a ...any) any {
				tmp0 := _a[0]
				var o OrdersItem
				if tmp0 != nil {
					o = tmp0.(OrdersItem)
				}
				_ = o
				return o.CustomerId
			}, rightKey: func(_v any) any { c := _v.(CustomersItem); _ = c; return c.Id }, left: true, right: true},
		}, _queryOpts{selectFn: func(_a ...any) any {
			tmp0 := _a[0]
			var o OrdersItem
			if tmp0 != nil {
				o = tmp0.(OrdersItem)
			}
			_ = o
			tmp1 := _a[1]
			var c CustomersItem
			if tmp1 != nil {
				c = tmp1.(CustomersItem)
			}
			_ = c
			return Result{
				o,
				c,
			}
		}, skip: -1, take: -1})
		out := make([]Result, len(resAny))
		for i, v := range resAny {
			out[i] = v.(Result)
		}
		return out
	}()
	fmt.Println("--- Outer Join using syntax ---")
	for _, row := range result {
		if _exists(row.Order) {
			if _exists(row.Customer) {
				_print("Order", _toAnyMap(row.Order)["id"], "by", _toAnyMap(row.Customer)["name"], "- $", _toAnyMap(row.Order)["total"])
			} else {
				_print("Order", _toAnyMap(row.Order)["id"], "by", "Unknown", "- $", _toAnyMap(row.Order)["total"])
			}
		} else {
			_print("Customer", _toAnyMap(row.Customer)["name"], "has no orders")
		}
	}
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
	case map[string]int:
		return len(s) > 0
	case string:
		return len([]rune(s)) > 0
	}
	rv := reflect.ValueOf(v)
	switch rv.Kind() {
	case reflect.Slice, reflect.Array:
		return rv.Len() > 0
	case reflect.Map:
		return !rv.IsNil() && rv.Len() > 0
	case reflect.Pointer:
		return !rv.IsNil()
	case reflect.Struct:
		return !rv.IsZero()
	}
	return false
}

func _print(args ...any) {
	first := true
	for _, a := range args {
		if !first {
			fmt.Print(" ")
		}
		first = false
		rv := reflect.ValueOf(a)
		if a == nil || ((rv.Kind() == reflect.Map || rv.Kind() == reflect.Slice) && rv.IsNil()) {
			fmt.Print("<nil>")
			continue
		}
		if rv.Kind() == reflect.Slice && rv.Type().Elem().Kind() != reflect.Uint8 {
			for i := 0; i < rv.Len(); i++ {
				if i > 0 {
					fmt.Print(" ")
				}
				fmt.Print(_sprint(rv.Index(i).Interface()))
			}
			continue
		}
		fmt.Print(_sprint(a))
	}
	fmt.Println()
}

type _joinSpec struct {
	items    []any
	on       func(...any) bool
	leftKey  func(...any) any
	rightKey func(any) any
	left     bool
	right    bool
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
		if j.leftKey != nil && j.rightKey != nil {
			if j.right && !j.left {
				lmap := map[string][]int{}
				for li, l := range items {
					key := fmt.Sprint(j.leftKey(l...))
					lmap[key] = append(lmap[key], li)
				}
				joined := [][]any{}
				for _, right := range j.items {
					key := fmt.Sprint(j.rightKey(right))
					if is, ok := lmap[key]; ok {
						for _, li := range is {
							left := items[li]
							keep := true
							if j.on != nil {
								args := append(append([]any(nil), left...), right)
								keep = j.on(args...)
							}
							if !keep {
								continue
							}
							joined = append(joined, append(append([]any(nil), left...), right))
						}
					} else {
						undef := make([]any, len(items[0]))
						joined = append(joined, append(undef, right))
					}
				}
				items = joined
				continue
			}
			rmap := map[string][]int{}
			for ri, r := range j.items {
				key := fmt.Sprint(j.rightKey(r))
				rmap[key] = append(rmap[key], ri)
			}
			joined := [][]any{}
			matched := make([]bool, len(j.items))
			for _, left := range items {
				key := fmt.Sprint(j.leftKey(left...))
				if is, ok := rmap[key]; ok {
					m := false
					for _, ri := range is {
						right := j.items[ri]
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
					if j.left && !m {
						joined = append(joined, append(append([]any(nil), left...), nil))
					}
				} else if j.left {
					joined = append(joined, append(append([]any(nil), left...), nil))
				}
			}
			if j.right {
				lw := 0
				if len(items) > 0 {
					lw = len(items[0])
				}
				for ri, right := range j.items {
					if !matched[ri] {
						undef := make([]any, lw)
						joined = append(joined, append(undef, right))
					}
				}
			}
			items = joined
			continue
		}
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

func _toAnyMap(m any) map[string]any {
	switch v := m.(type) {
	case map[string]any:
		return v
	case map[string]string:
		out := make(map[string]any, len(v))
		for k, vv := range v {
			out[k] = vv
		}
		return out
	default:
		rv := reflect.ValueOf(v)
		if rv.Kind() == reflect.Struct {
			out := make(map[string]any, rv.NumField())
			rt := rv.Type()
			for i := 0; i < rv.NumField(); i++ {
				name := rt.Field(i).Name
				if tag := rt.Field(i).Tag.Get("json"); tag != "" {
					comma := strings.Index(tag, ",")
					if comma >= 0 {
						tag = tag[:comma]
					}
					if tag != "-" {
						name = tag
					}
				}
				out[name] = rv.Field(i).Interface()
			}
			return out
		}
		return nil
	}
}

func _toAnySlice[T any](s []T) []any {
	out := []any{}
	for _, v := range s {
		out = append(out, v)
	}
	return out
}
