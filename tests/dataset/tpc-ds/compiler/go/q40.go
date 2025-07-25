//go:build ignore

// Generated by Mochi compiler v0.10.26 on 2025-07-16T01:05:33Z

package main

import (
	"encoding/json"
	"fmt"
	"mochi/runtime/data"
	"reflect"
	"sort"
	"time"

	"golang.org/x/exp/constraints"
)

type Catalog_return struct {
	Order    int     `json:"order"`
	Item_sk  int     `json:"item_sk"`
	Refunded float64 `json:"refunded"`
}

type Catalog_sale struct {
	Order        int     `json:"order"`
	Item_sk      int     `json:"item_sk"`
	Warehouse_sk int     `json:"warehouse_sk"`
	Date_sk      int     `json:"date_sk"`
	Price        float64 `json:"price"`
}

type Date_dim struct {
	Date_sk int    `json:"date_sk"`
	Date    string `json:"date"`
}

type GKey struct {
	W_state   string `json:"w_state"`
	I_item_id string `json:"i_item_id"`
}

type Item struct {
	Item_sk       int     `json:"item_sk"`
	Item_id       string  `json:"item_id"`
	Current_price float64 `json:"current_price"`
}

type Record struct {
	W_state   string  `json:"w_state"`
	I_item_id string  `json:"i_item_id"`
	Sold_date string  `json:"sold_date"`
	Net       float64 `json:"net"`
}

type Result struct {
	W_state      any     `json:"w_state"`
	I_item_id    any     `json:"i_item_id"`
	Sales_before float64 `json:"sales_before"`
	Sales_after  float64 `json:"sales_after"`
}

type Warehouse struct {
	Warehouse_sk int    `json:"warehouse_sk"`
	State        string `json:"state"`
}

type v = Result

func expect(cond bool) {
	if !cond {
		panic("expect failed")
	}
}

func formatDuration(d time.Duration) string {
	switch {
	case d < time.Microsecond:
		return fmt.Sprintf("%dns", d.Nanoseconds())
	case d < time.Millisecond:
		return fmt.Sprintf("%.1fµs", float64(d.Microseconds()))
	case d < time.Second:
		return fmt.Sprintf("%.1fms", float64(d.Milliseconds()))
	default:
		return fmt.Sprintf("%.2fs", d.Seconds())
	}
}

func printTestStart(name string) {
	fmt.Printf("   test %-30s ...", name)
}

func printTestPass(d time.Duration) {
	fmt.Printf(" ok (%s)\n", formatDuration(d))
}

func printTestFail(err error, d time.Duration) {
	fmt.Printf(" fail %v (%s)\n", err, formatDuration(d))
}

func test_TPCDS_Q40_simplified() {
	expect(_equal(result, []v{v{
		W_state:      "CA",
		I_item_id:    "I1",
		Sales_before: 100.0,
		Sales_after:  0.0,
	}}))
}

var catalog_sales []Catalog_sale
var catalog_returns []Catalog_return
var item []Item
var warehouse []Warehouse
var date_dim []Date_dim
var sales_date string
var records []Record
var result []Result

func main() {
	catalog_sales = []Catalog_sale{Catalog_sale{
		Order:        1,
		Item_sk:      1,
		Warehouse_sk: 1,
		Date_sk:      1,
		Price:        100.0,
	}, Catalog_sale{
		Order:        2,
		Item_sk:      1,
		Warehouse_sk: 1,
		Date_sk:      2,
		Price:        150.0,
	}}
	catalog_returns = []Catalog_return{Catalog_return{
		Order:    2,
		Item_sk:  1,
		Refunded: 150.0,
	}}
	item = []Item{Item{
		Item_sk:       1,
		Item_id:       "I1",
		Current_price: 1.2,
	}}
	warehouse = []Warehouse{Warehouse{
		Warehouse_sk: 1,
		State:        "CA",
	}}
	date_dim = []Date_dim{Date_dim{
		Date_sk: 1,
		Date:    "2020-01-10",
	}, Date_dim{
		Date_sk: 2,
		Date:    "2020-01-20",
	}}
	sales_date = "2020-01-15"
	records = func() []Record {
		src := _toAnySlice(catalog_sales)
		resAny := _query(src, []_joinSpec{
			{items: _toAnySlice(catalog_returns), on: func(_a ...any) bool {
				tmp0 := _a[0]
				var cs Catalog_sale
				if tmp0 != nil {
					cs = tmp0.(Catalog_sale)
				}
				_ = cs
				tmp1 := _a[1]
				var cr Catalog_return
				if tmp1 != nil {
					cr = tmp1.(Catalog_return)
				}
				_ = cr
				return ((cs.Order == cr.Order) && (cs.Item_sk == cr.Item_sk))
			}, left: true},
			{items: _toAnySlice(warehouse), on: func(_a ...any) bool {
				tmp0 := _a[0]
				var cs Catalog_sale
				if tmp0 != nil {
					cs = tmp0.(Catalog_sale)
				}
				_ = cs
				tmp1 := _a[1]
				var cr Catalog_return
				if tmp1 != nil {
					cr = tmp1.(Catalog_return)
				}
				_ = cr
				tmp2 := _a[2]
				var w Warehouse
				if tmp2 != nil {
					w = tmp2.(Warehouse)
				}
				_ = w
				return (cs.Warehouse_sk == w.Warehouse_sk)
			}, leftKey: func(_a ...any) any {
				tmp0 := _a[0]
				var cs Catalog_sale
				if tmp0 != nil {
					cs = tmp0.(Catalog_sale)
				}
				_ = cs
				tmp1 := _a[1]
				var cr Catalog_return
				if tmp1 != nil {
					cr = tmp1.(Catalog_return)
				}
				_ = cr
				return cs.Warehouse_sk
			}, rightKey: func(_v any) any { w := _v.(Warehouse); _ = w; return w.Warehouse_sk }},
			{items: _toAnySlice(item), on: func(_a ...any) bool {
				tmp0 := _a[0]
				var cs Catalog_sale
				if tmp0 != nil {
					cs = tmp0.(Catalog_sale)
				}
				_ = cs
				tmp1 := _a[1]
				var cr Catalog_return
				if tmp1 != nil {
					cr = tmp1.(Catalog_return)
				}
				_ = cr
				tmp2 := _a[2]
				var w Warehouse
				if tmp2 != nil {
					w = tmp2.(Warehouse)
				}
				_ = w
				tmp3 := _a[3]
				var i Item
				if tmp3 != nil {
					i = tmp3.(Item)
				}
				_ = i
				return (cs.Item_sk == i.Item_sk)
			}, leftKey: func(_a ...any) any {
				tmp0 := _a[0]
				var cs Catalog_sale
				if tmp0 != nil {
					cs = tmp0.(Catalog_sale)
				}
				_ = cs
				tmp1 := _a[1]
				var cr Catalog_return
				if tmp1 != nil {
					cr = tmp1.(Catalog_return)
				}
				_ = cr
				tmp2 := _a[2]
				var w Warehouse
				if tmp2 != nil {
					w = tmp2.(Warehouse)
				}
				_ = w
				return cs.Item_sk
			}, rightKey: func(_v any) any { i := _v.(Item); _ = i; return i.Item_sk }},
			{items: _toAnySlice(date_dim), on: func(_a ...any) bool {
				tmp0 := _a[0]
				var cs Catalog_sale
				if tmp0 != nil {
					cs = tmp0.(Catalog_sale)
				}
				_ = cs
				tmp1 := _a[1]
				var cr Catalog_return
				if tmp1 != nil {
					cr = tmp1.(Catalog_return)
				}
				_ = cr
				tmp2 := _a[2]
				var w Warehouse
				if tmp2 != nil {
					w = tmp2.(Warehouse)
				}
				_ = w
				tmp3 := _a[3]
				var i Item
				if tmp3 != nil {
					i = tmp3.(Item)
				}
				_ = i
				tmp4 := _a[4]
				var d Date_dim
				if tmp4 != nil {
					d = tmp4.(Date_dim)
				}
				_ = d
				return (cs.Date_sk == d.Date_sk)
			}, leftKey: func(_a ...any) any {
				tmp0 := _a[0]
				var cs Catalog_sale
				if tmp0 != nil {
					cs = tmp0.(Catalog_sale)
				}
				_ = cs
				tmp1 := _a[1]
				var cr Catalog_return
				if tmp1 != nil {
					cr = tmp1.(Catalog_return)
				}
				_ = cr
				tmp2 := _a[2]
				var w Warehouse
				if tmp2 != nil {
					w = tmp2.(Warehouse)
				}
				_ = w
				tmp3 := _a[3]
				var i Item
				if tmp3 != nil {
					i = tmp3.(Item)
				}
				_ = i
				return cs.Date_sk
			}, rightKey: func(_v any) any { d := _v.(Date_dim); _ = d; return d.Date_sk }},
		}, _queryOpts{selectFn: func(_a ...any) any {
			tmp0 := _a[0]
			var cs Catalog_sale
			if tmp0 != nil {
				cs = tmp0.(Catalog_sale)
			}
			_ = cs
			tmp1 := _a[1]
			var cr Catalog_return
			if tmp1 != nil {
				cr = tmp1.(Catalog_return)
			}
			_ = cr
			tmp2 := _a[2]
			var w Warehouse
			if tmp2 != nil {
				w = tmp2.(Warehouse)
			}
			_ = w
			tmp3 := _a[3]
			var i Item
			if tmp3 != nil {
				i = tmp3.(Item)
			}
			_ = i
			tmp4 := _a[4]
			var d Date_dim
			if tmp4 != nil {
				d = tmp4.(Date_dim)
			}
			_ = d
			return Record{
				W_state:   w.State,
				I_item_id: i.Item_id,
				Sold_date: d.Date,
				Net: (cs.Price - (func() float64 {
					if _equal(cr, nil) {
						return 0.0
					} else {
						return cr.Refunded
					}
				}())),
			}
		}, where: func(_a ...any) bool {
			tmp0 := _a[0]
			var cs Catalog_sale
			if tmp0 != nil {
				cs = tmp0.(Catalog_sale)
			}
			_ = cs
			tmp1 := _a[1]
			var cr Catalog_return
			if tmp1 != nil {
				cr = tmp1.(Catalog_return)
			}
			_ = cr
			tmp2 := _a[2]
			var w Warehouse
			if tmp2 != nil {
				w = tmp2.(Warehouse)
			}
			_ = w
			tmp3 := _a[3]
			var i Item
			if tmp3 != nil {
				i = tmp3.(Item)
			}
			_ = i
			tmp4 := _a[4]
			var d Date_dim
			if tmp4 != nil {
				d = tmp4.(Date_dim)
			}
			_ = d
			return ((i.Current_price >= 0.99) && (i.Current_price <= 1.49))
		}, skip: -1, take: -1})
		out := make([]Record, len(resAny))
		for i, v := range resAny {
			out[i] = v.(Record)
		}
		return out
	}()
	result = func() []Result {
		groups := map[string]*data.Group{}
		order := []string{}
		for _, r := range records {
			key := GKey{
				W_state:   r.W_state,
				I_item_id: r.I_item_id,
			}
			ks := fmt.Sprint(key)
			g, ok := groups[ks]
			if !ok {
				g = &data.Group{Key: key}
				groups[ks] = g
				order = append(order, ks)
			}
			g.Items = append(g.Items, r)
		}
		results := []Result{}
		for _, ks := range order {
			g := groups[ks]
			results = append(results, Result{
				W_state:   g.Key.(GKey).W_state,
				I_item_id: g.Key.(GKey).I_item_id,
				Sales_before: _sumOrdered[float64](func() []float64 {
					results := []float64{}
					for _, xRaw := range g.Items {
						x := xRaw.(Record)
						results = append(results, func() float64 {
							if x.Sold_date < sales_date {
								return x.Net
							} else {
								return 0.0
							}
						}())
					}
					return results
				}()),
				Sales_after: _sumOrdered[float64](func() []float64 {
					results := []float64{}
					for _, xRaw := range g.Items {
						x := xRaw.(Record)
						results = append(results, func() float64 {
							if x.Sold_date >= sales_date {
								return x.Net
							} else {
								return 0.0
							}
						}())
					}
					return results
				}()),
			})
		}
		return results
	}()
	func() { b, _ := json.Marshal(result); fmt.Println(string(b)) }()
	test_TPCDS_Q40_simplified()
}

func _equal(a, b any) bool {
	av := reflect.ValueOf(a)
	bv := reflect.ValueOf(b)
	if av.Kind() == reflect.Slice && bv.Kind() == reflect.Slice {
		if av.Len() != bv.Len() {
			return false
		}
		for i := 0; i < av.Len(); i++ {
			if !_equal(av.Index(i).Interface(), bv.Index(i).Interface()) {
				return false
			}
		}
		return true
	}
	if av.Kind() == reflect.Map && bv.Kind() == reflect.Map {
		if av.Len() != bv.Len() {
			return false
		}
		for _, k := range av.MapKeys() {
			bvVal := bv.MapIndex(k)
			if !bvVal.IsValid() {
				return false
			}
			if !_equal(av.MapIndex(k).Interface(), bvVal.Interface()) {
				return false
			}
		}
		return true
	}
	if (av.Kind() == reflect.Int || av.Kind() == reflect.Int64 || av.Kind() == reflect.Float64) &&
		(bv.Kind() == reflect.Int || bv.Kind() == reflect.Int64 || bv.Kind() == reflect.Float64) {
		return av.Convert(reflect.TypeOf(float64(0))).Float() == bv.Convert(reflect.TypeOf(float64(0))).Float()
	}
	return reflect.DeepEqual(a, b)
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

func _sumOrdered[T constraints.Integer | constraints.Float](s []T) float64 {
	var sum float64
	for _, v := range s {
		sum += float64(v)
	}
	return sum
}

func _toAnySlice[T any](s []T) []any {
	out := make([]any, len(s))
	for i, v := range s {
		out[i] = v
	}
	return out
}
