//go:build ignore

// Generated by Mochi compiler v0.10.26 on 2025-07-16T01:05:20Z

package main

import (
	"encoding/json"
	"fmt"
	"mochi/runtime/data"
	"reflect"
	"sort"
	"time"
)

type Customer struct {
	C_customer_sk         int    `json:"c_customer_sk"`
	C_last_name           string `json:"c_last_name"`
	C_first_name          string `json:"c_first_name"`
	C_salutation          string `json:"c_salutation"`
	C_preferred_cust_flag string `json:"c_preferred_cust_flag"`
}

type Date_dim struct {
	D_date_sk int `json:"d_date_sk"`
	D_dom     int `json:"d_dom"`
	D_year    int `json:"d_year"`
}

type Dn struct {
	Ss_ticket_number any `json:"ss_ticket_number"`
	Ss_customer_sk   any `json:"ss_customer_sk"`
	Cnt              int `json:"cnt"`
}

type GKey struct {
	Ticket int `json:"ticket"`
	Cust   int `json:"cust"`
}

type Household_demographic struct {
	Hd_demo_sk       int    `json:"hd_demo_sk"`
	Hd_buy_potential string `json:"hd_buy_potential"`
	Hd_vehicle_count int    `json:"hd_vehicle_count"`
	Hd_dep_count     int    `json:"hd_dep_count"`
}

type Result struct {
	C_last_name           string `json:"c_last_name"`
	C_first_name          string `json:"c_first_name"`
	C_salutation          string `json:"c_salutation"`
	C_preferred_cust_flag string `json:"c_preferred_cust_flag"`
	Ss_ticket_number      any    `json:"ss_ticket_number"`
	Cnt                   int    `json:"cnt"`
}

type Store struct {
	S_store_sk int    `json:"s_store_sk"`
	S_county   string `json:"s_county"`
}

type Store_sale struct {
	Ss_ticket_number int `json:"ss_ticket_number"`
	Ss_customer_sk   int `json:"ss_customer_sk"`
	Ss_sold_date_sk  int `json:"ss_sold_date_sk"`
	Ss_store_sk      int `json:"ss_store_sk"`
	Ss_hdemo_sk      int `json:"ss_hdemo_sk"`
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

func test_TPCDS_Q34_simplified() {
	expect(_equal(result, []v{v{
		C_last_name:           "Smith",
		C_first_name:          "John",
		C_salutation:          "Mr.",
		C_preferred_cust_flag: "Y",
		Ss_ticket_number:      1,
		Cnt:                   16,
	}}))
}

var store_sales []Store_sale
var date_dim []Date_dim
var store []Store
var household_demographics []Household_demographic
var customer []Customer
var dn []Dn
var result []Result

func main() {
	store_sales = []Store_sale{
		Store_sale{
			Ss_ticket_number: 1,
			Ss_customer_sk:   1,
			Ss_sold_date_sk:  1,
			Ss_store_sk:      1,
			Ss_hdemo_sk:      1,
		},
		Store_sale{
			Ss_ticket_number: 1,
			Ss_customer_sk:   1,
			Ss_sold_date_sk:  1,
			Ss_store_sk:      1,
			Ss_hdemo_sk:      1,
		},
		Store_sale{
			Ss_ticket_number: 1,
			Ss_customer_sk:   1,
			Ss_sold_date_sk:  1,
			Ss_store_sk:      1,
			Ss_hdemo_sk:      1,
		},
		Store_sale{
			Ss_ticket_number: 1,
			Ss_customer_sk:   1,
			Ss_sold_date_sk:  1,
			Ss_store_sk:      1,
			Ss_hdemo_sk:      1,
		},
		Store_sale{
			Ss_ticket_number: 1,
			Ss_customer_sk:   1,
			Ss_sold_date_sk:  1,
			Ss_store_sk:      1,
			Ss_hdemo_sk:      1,
		},
		Store_sale{
			Ss_ticket_number: 1,
			Ss_customer_sk:   1,
			Ss_sold_date_sk:  1,
			Ss_store_sk:      1,
			Ss_hdemo_sk:      1,
		},
		Store_sale{
			Ss_ticket_number: 1,
			Ss_customer_sk:   1,
			Ss_sold_date_sk:  1,
			Ss_store_sk:      1,
			Ss_hdemo_sk:      1,
		},
		Store_sale{
			Ss_ticket_number: 1,
			Ss_customer_sk:   1,
			Ss_sold_date_sk:  1,
			Ss_store_sk:      1,
			Ss_hdemo_sk:      1,
		},
		Store_sale{
			Ss_ticket_number: 1,
			Ss_customer_sk:   1,
			Ss_sold_date_sk:  1,
			Ss_store_sk:      1,
			Ss_hdemo_sk:      1,
		},
		Store_sale{
			Ss_ticket_number: 1,
			Ss_customer_sk:   1,
			Ss_sold_date_sk:  1,
			Ss_store_sk:      1,
			Ss_hdemo_sk:      1,
		},
		Store_sale{
			Ss_ticket_number: 1,
			Ss_customer_sk:   1,
			Ss_sold_date_sk:  1,
			Ss_store_sk:      1,
			Ss_hdemo_sk:      1,
		},
		Store_sale{
			Ss_ticket_number: 1,
			Ss_customer_sk:   1,
			Ss_sold_date_sk:  1,
			Ss_store_sk:      1,
			Ss_hdemo_sk:      1,
		},
		Store_sale{
			Ss_ticket_number: 1,
			Ss_customer_sk:   1,
			Ss_sold_date_sk:  1,
			Ss_store_sk:      1,
			Ss_hdemo_sk:      1,
		},
		Store_sale{
			Ss_ticket_number: 1,
			Ss_customer_sk:   1,
			Ss_sold_date_sk:  1,
			Ss_store_sk:      1,
			Ss_hdemo_sk:      1,
		},
		Store_sale{
			Ss_ticket_number: 1,
			Ss_customer_sk:   1,
			Ss_sold_date_sk:  1,
			Ss_store_sk:      1,
			Ss_hdemo_sk:      1,
		},
		Store_sale{
			Ss_ticket_number: 1,
			Ss_customer_sk:   1,
			Ss_sold_date_sk:  1,
			Ss_store_sk:      1,
			Ss_hdemo_sk:      1,
		},
		Store_sale{
			Ss_ticket_number: 2,
			Ss_customer_sk:   2,
			Ss_sold_date_sk:  1,
			Ss_store_sk:      1,
			Ss_hdemo_sk:      2,
		},
		Store_sale{
			Ss_ticket_number: 2,
			Ss_customer_sk:   2,
			Ss_sold_date_sk:  1,
			Ss_store_sk:      1,
			Ss_hdemo_sk:      2,
		},
		Store_sale{
			Ss_ticket_number: 2,
			Ss_customer_sk:   2,
			Ss_sold_date_sk:  1,
			Ss_store_sk:      1,
			Ss_hdemo_sk:      2,
		},
		Store_sale{
			Ss_ticket_number: 2,
			Ss_customer_sk:   2,
			Ss_sold_date_sk:  1,
			Ss_store_sk:      1,
			Ss_hdemo_sk:      2,
		},
		Store_sale{
			Ss_ticket_number: 2,
			Ss_customer_sk:   2,
			Ss_sold_date_sk:  1,
			Ss_store_sk:      1,
			Ss_hdemo_sk:      2,
		},
		Store_sale{
			Ss_ticket_number: 2,
			Ss_customer_sk:   2,
			Ss_sold_date_sk:  1,
			Ss_store_sk:      1,
			Ss_hdemo_sk:      2,
		},
		Store_sale{
			Ss_ticket_number: 2,
			Ss_customer_sk:   2,
			Ss_sold_date_sk:  1,
			Ss_store_sk:      1,
			Ss_hdemo_sk:      2,
		},
		Store_sale{
			Ss_ticket_number: 2,
			Ss_customer_sk:   2,
			Ss_sold_date_sk:  1,
			Ss_store_sk:      1,
			Ss_hdemo_sk:      2,
		},
		Store_sale{
			Ss_ticket_number: 2,
			Ss_customer_sk:   2,
			Ss_sold_date_sk:  1,
			Ss_store_sk:      1,
			Ss_hdemo_sk:      2,
		},
		Store_sale{
			Ss_ticket_number: 2,
			Ss_customer_sk:   2,
			Ss_sold_date_sk:  1,
			Ss_store_sk:      1,
			Ss_hdemo_sk:      2,
		},
	}
	date_dim = []Date_dim{Date_dim{
		D_date_sk: 1,
		D_dom:     2,
		D_year:    2000,
	}}
	store = []Store{Store{
		S_store_sk: 1,
		S_county:   "A",
	}}
	household_demographics = []Household_demographic{Household_demographic{
		Hd_demo_sk:       1,
		Hd_buy_potential: ">10000",
		Hd_vehicle_count: 2,
		Hd_dep_count:     3,
	}, Household_demographic{
		Hd_demo_sk:       2,
		Hd_buy_potential: ">10000",
		Hd_vehicle_count: 2,
		Hd_dep_count:     1,
	}}
	customer = []Customer{Customer{
		C_customer_sk:         1,
		C_last_name:           "Smith",
		C_first_name:          "John",
		C_salutation:          "Mr.",
		C_preferred_cust_flag: "Y",
	}, Customer{
		C_customer_sk:         2,
		C_last_name:           "Jones",
		C_first_name:          "Alice",
		C_salutation:          "Ms.",
		C_preferred_cust_flag: "N",
	}}
	dn = func() []Dn {
		groups := map[string]*data.Group{}
		order := []string{}
		for _, ss := range store_sales {
			for _, d := range date_dim {
				if !(ss.Ss_sold_date_sk == d.D_date_sk) {
					continue
				}
				for _, s := range store {
					if !(ss.Ss_store_sk == s.S_store_sk) {
						continue
					}
					for _, hd := range household_demographics {
						if !(ss.Ss_hdemo_sk == hd.Hd_demo_sk) {
							continue
						}
						if ((((((d.D_dom >= 1) && (d.D_dom <= 3)) && (hd.Hd_buy_potential == ">10000")) && (hd.Hd_vehicle_count > 0)) && ((float64(hd.Hd_dep_count) / float64(hd.Hd_vehicle_count)) > 1.2)) && (d.D_year == 2000)) && (s.S_county == "A") {
							key := GKey{
								Ticket: ss.Ss_ticket_number,
								Cust:   ss.Ss_customer_sk,
							}
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
			}
		}
		items := []*data.Group{}
		for _, ks := range order {
			items = append(items, groups[ks])
		}
		results := []Dn{}
		for _, g := range items {
			results = append(results, Dn{
				Ss_ticket_number: g.Key.(GKey).Ticket,
				Ss_customer_sk:   g.Key.(GKey).Cust,
				Cnt:              len(g.Items),
			})
		}
		return results
	}()
	result = func() []Result {
		src := _toAnySlice(dn)
		resAny := _query(src, []_joinSpec{
			{items: _toAnySlice(customer), on: func(_a ...any) bool {
				tmp0 := _a[0]
				var dn1 Dn
				if tmp0 != nil {
					dn1 = tmp0.(Dn)
				}
				_ = dn1
				tmp1 := _a[1]
				var c Customer
				if tmp1 != nil {
					c = tmp1.(Customer)
				}
				_ = c
				return _equal(dn1.Ss_customer_sk, c.C_customer_sk)
			}, leftKey: func(_a ...any) any {
				tmp0 := _a[0]
				var dn1 Dn
				if tmp0 != nil {
					dn1 = tmp0.(Dn)
				}
				_ = dn1
				return dn1.Ss_customer_sk
			}, rightKey: func(_v any) any { c := _v.(Customer); _ = c; return c.C_customer_sk }},
		}, _queryOpts{selectFn: func(_a ...any) any {
			tmp0 := _a[0]
			var dn1 Dn
			if tmp0 != nil {
				dn1 = tmp0.(Dn)
			}
			_ = dn1
			tmp1 := _a[1]
			var c Customer
			if tmp1 != nil {
				c = tmp1.(Customer)
			}
			_ = c
			return Result{
				C_last_name:           c.C_last_name,
				C_first_name:          c.C_first_name,
				C_salutation:          c.C_salutation,
				C_preferred_cust_flag: c.C_preferred_cust_flag,
				Ss_ticket_number:      dn1.Ss_ticket_number,
				Cnt:                   dn1.Cnt,
			}
		}, where: func(_a ...any) bool {
			tmp0 := _a[0]
			var dn1 Dn
			if tmp0 != nil {
				dn1 = tmp0.(Dn)
			}
			_ = dn1
			tmp1 := _a[1]
			var c Customer
			if tmp1 != nil {
				c = tmp1.(Customer)
			}
			_ = c
			return ((dn1.Cnt >= 15) && (dn1.Cnt <= 20))
		}, sortKey: func(_a ...any) any {
			tmp0 := _a[0]
			var dn1 Dn
			if tmp0 != nil {
				dn1 = tmp0.(Dn)
			}
			_ = dn1
			tmp1 := _a[1]
			var c Customer
			if tmp1 != nil {
				c = tmp1.(Customer)
			}
			_ = c
			return c.C_last_name
		}, skip: -1, take: -1})
		out := make([]Result, len(resAny))
		for i, v := range resAny {
			out[i] = v.(Result)
		}
		return out
	}()
	func() { b, _ := json.Marshal(result); fmt.Println(string(b)) }()
	test_TPCDS_Q34_simplified()
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

func _toAnySlice[T any](s []T) []any {
	out := make([]any, len(s))
	for i, v := range s {
		out[i] = v
	}
	return out
}
