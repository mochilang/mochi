//go:build ignore

package main

import (
	"fmt"
	"reflect"
	"sort"
	"strings"
)

func main() {
	type CustomersItem struct {
		Id   int    `json:"id"`
		Name string `json:"name"`
	}

	var customers []CustomersItem = []CustomersItem{CustomersItem{
		Id:   1,
		Name: "Alice",
	}, CustomersItem{
		Id:   2,
		Name: "Bob",
	}}
	_ = customers
	type OrdersItem struct {
		Id         int `json:"id"`
		CustomerId int `json:"customerId"`
	}

	var orders []OrdersItem = []OrdersItem{OrdersItem{
		Id:         100,
		CustomerId: 1,
	}, OrdersItem{
		Id:         101,
		CustomerId: 2,
	}}
	type ItemsItem struct {
		OrderId int    `json:"orderId"`
		Sku     string `json:"sku"`
	}

	var items []ItemsItem = []ItemsItem{ItemsItem{
		OrderId: 100,
		Sku:     "a",
	}}
	_ = items
	type Result struct {
		OrderId any `json:"orderId"`
		Name    any `json:"name"`
		Item    any `json:"item"`
	}

	var result []Result = func() []Result {
		src := _toAnySlice(orders)
		resAny := _query(src, []_joinSpec{
			{items: _toAnySlice(customers), on: func(_a ...any) bool {
				_tmp0 := _a[0]
				var o OrdersItem
				if _tmp0 != nil {
					o = _tmp0.(OrdersItem)
				}
				_ = o
				_tmp1 := _a[1]
				var c CustomersItem
				if _tmp1 != nil {
					c = _tmp1.(CustomersItem)
				}
				_ = c
				return (o.CustomerId == c.Id)
			}, leftKey: func(_a ...any) any {
				_tmp0 := _a[0]
				var o OrdersItem
				if _tmp0 != nil {
					o = _tmp0.(OrdersItem)
				}
				_ = o
				return o.CustomerId
			}, rightKey: func(_v any) any { c := _v.(CustomersItem); _ = c; return c.Id }},
			{items: _toAnySlice(items), on: func(_a ...any) bool {
				_tmp0 := _a[0]
				var o OrdersItem
				if _tmp0 != nil {
					o = _tmp0.(OrdersItem)
				}
				_ = o
				_tmp1 := _a[1]
				var c CustomersItem
				if _tmp1 != nil {
					c = _tmp1.(CustomersItem)
				}
				_ = c
				_tmp2 := _a[2]
				var i ItemsItem
				if _tmp2 != nil {
					i = _tmp2.(ItemsItem)
				}
				_ = i
				return (o.Id == i.OrderId)
			}, leftKey: func(_a ...any) any {
				_tmp0 := _a[0]
				var o OrdersItem
				if _tmp0 != nil {
					o = _tmp0.(OrdersItem)
				}
				_ = o
				_tmp1 := _a[1]
				var c CustomersItem
				if _tmp1 != nil {
					c = _tmp1.(CustomersItem)
				}
				_ = c
				return o.Id
			}, rightKey: func(_v any) any { i := _v.(ItemsItem); _ = i; return i.OrderId }, left: true},
		}, _queryOpts{selectFn: func(_a ...any) any {
			_tmp0 := _a[0]
			var o OrdersItem
			if _tmp0 != nil {
				o = _tmp0.(OrdersItem)
			}
			_ = o
			_tmp1 := _a[1]
			var c CustomersItem
			if _tmp1 != nil {
				c = _tmp1.(CustomersItem)
			}
			_ = c
			_tmp2 := _a[2]
			var i ItemsItem
			if _tmp2 != nil {
				i = _tmp2.(ItemsItem)
			}
			_ = i
			return Result{
				OrderId: o.Id,
				Name:    c.Name,
				Item:    i,
			}
		}, skip: -1, take: -1})
		out := make([]Result, len(resAny))
		for i, v := range resAny {
			out[i] = v.(Result)
		}
		return out
	}()
	fmt.Println(_sprint("--- Left Join Multi ---"))
	for _, r := range result {
		fmt.Println(strings.TrimRight(strings.Join([]string{_sprint(r.OrderId), _sprint(r.Name), _sprint(r.Item)}, " "), " "))
	}
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

func _toAnySlice[T any](s []T) []any {
	out := []any{}
	for _, v := range s {
		out = append(out, v)
	}
	return out
}
