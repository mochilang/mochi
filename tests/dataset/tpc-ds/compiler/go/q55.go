//go:build ignore

// Generated by Mochi compiler v0.10.26 on 2025-07-16T01:05:47Z

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

type Date_dim struct {
	D_date_sk int `json:"d_date_sk"`
	D_year    int `json:"d_year"`
	D_moy     int `json:"d_moy"`
}

type GKey struct {
	Brand_id int `json:"brand_id"`
}

type Grouped struct {
	Brand_id  any     `json:"brand_id"`
	Ext_price float64 `json:"ext_price"`
}

type Item struct {
	I_item_sk    int `json:"i_item_sk"`
	I_brand_id   int `json:"i_brand_id"`
	I_manager_id int `json:"i_manager_id"`
}

type Store_sale struct {
	Item      int     `json:"item"`
	Sold_date int     `json:"sold_date"`
	Price     float64 `json:"price"`
}

type v map[string]any

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

func test_TPCDS_Q55_simplified() {
	expect(_equal(result, []v{v{
		Brand_id:  10,
		Ext_price: 35.0,
	}, v{
		Brand_id:  20,
		Ext_price: 20.0,
	}}))
}

var store_sales []Store_sale
var item []Item
var date_dim []Date_dim
var grouped []Grouped
var result []Grouped

func main() {
	store_sales = []Store_sale{Store_sale{
		Item:      1,
		Sold_date: 1,
		Price:     10.0,
	}, Store_sale{
		Item:      2,
		Sold_date: 1,
		Price:     20.0,
	}, Store_sale{
		Item:      3,
		Sold_date: 1,
		Price:     25.0,
	}}
	item = []Item{Item{
		I_item_sk:    1,
		I_brand_id:   10,
		I_manager_id: 1,
	}, Item{
		I_item_sk:    2,
		I_brand_id:   20,
		I_manager_id: 1,
	}, Item{
		I_item_sk:    3,
		I_brand_id:   10,
		I_manager_id: 1,
	}}
	date_dim = []Date_dim{Date_dim{
		D_date_sk: 1,
		D_year:    2001,
		D_moy:     11,
	}}
	grouped = func() []Grouped {
		groups := map[string]*data.Group{}
		order := []string{}
		for _, ss := range store_sales {
			for _, i := range item {
				if !((ss.Item == i.I_item_sk) && (i.I_manager_id == 1)) {
					continue
				}
				for _, d := range date_dim {
					if !(ss.Sold_date == d.D_date_sk) {
						continue
					}
					key := GKey{Brand_id: i.I_brand_id}
					ks := fmt.Sprint(key)
					g, ok := groups[ks]
					if !ok {
						g = &data.Group{Key: key}
						groups[ks] = g
						order = append(order, ks)
					}
					g.Items = append(g.Items, ss)
				}
			}
		}
		items := []*data.Group{}
		for _, ks := range order {
			items = append(items, groups[ks])
		}
		results := []Grouped{}
		for _, g := range items {
			results = append(results, Grouped{
				Brand_id: g.Key.(GKey).Brand_id,
				Ext_price: _sumOrdered[float64](func() []float64 {
					results := []float64{}
					for _, xRaw := range g.Items {
						x := xRaw.(Store_sale)
						results = append(results, x.Price)
					}
					return results
				}()),
			})
		}
		return results
	}()
	result = func() []Grouped {
		src := _toAnySlice(grouped)
		resAny := _query(src, []_joinSpec{}, _queryOpts{selectFn: func(_a ...any) any {
			tmp0 := _a[0]
			var g Grouped
			if tmp0 != nil {
				g = tmp0.(Grouped)
			}
			_ = g
			return g
		}, sortKey: func(_a ...any) any {
			tmp0 := _a[0]
			var g Grouped
			if tmp0 != nil {
				g = tmp0.(Grouped)
			}
			_ = g
			return []any{-g.Ext_price, g.Brand_id}
		}, skip: -1, take: -1})
		out := make([]Grouped, len(resAny))
		for i, v := range resAny {
			out[i] = v.(Grouped)
		}
		return out
	}()
	func() { b, _ := json.Marshal(result); fmt.Println(string(b)) }()
	test_TPCDS_Q55_simplified()
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
