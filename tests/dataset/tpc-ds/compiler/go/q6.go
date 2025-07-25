//go:build ignore

// Generated by Mochi compiler v0.10.26 on 2025-07-16T01:04:21Z

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

type Customer struct {
	C_customer_sk     int `json:"c_customer_sk"`
	C_current_addr_sk int `json:"c_current_addr_sk"`
}

type Customer_addres struct {
	Ca_address_sk int    `json:"ca_address_sk"`
	Ca_state      string `json:"ca_state"`
	Ca_zip        string `json:"ca_zip"`
}

type Date_dim struct {
	D_date_sk   int `json:"d_date_sk"`
	D_year      int `json:"d_year"`
	D_moy       int `json:"d_moy"`
	D_month_seq int `json:"d_month_seq"`
}

type Item struct {
	I_item_sk       int     `json:"i_item_sk"`
	I_category      string  `json:"i_category"`
	I_current_price float64 `json:"i_current_price"`
}

type Result struct {
	State any `json:"state"`
	Cnt   int `json:"cnt"`
}

type Store_sale struct {
	Ss_customer_sk  int `json:"ss_customer_sk"`
	Ss_sold_date_sk int `json:"ss_sold_date_sk"`
	Ss_item_sk      int `json:"ss_item_sk"`
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

func test_TPCDS_Q6_result() {
	expect(_equal(result, []v{v{
		State: "CA",
		Cnt:   10,
	}}))
}

var customer_address []Customer_addres
var customer []Customer
var store_sales []Store_sale
var date_dim []Date_dim
var item []Item
var target_month_seq int
var result []Result

func main() {
	customer_address = []Customer_addres{Customer_addres{
		Ca_address_sk: 1,
		Ca_state:      "CA",
		Ca_zip:        "12345",
	}}
	customer = []Customer{Customer{
		C_customer_sk:     1,
		C_current_addr_sk: 1,
	}}
	store_sales = []Store_sale{
		Store_sale{
			Ss_customer_sk:  1,
			Ss_sold_date_sk: 1,
			Ss_item_sk:      1,
		},
		Store_sale{
			Ss_customer_sk:  1,
			Ss_sold_date_sk: 1,
			Ss_item_sk:      1,
		},
		Store_sale{
			Ss_customer_sk:  1,
			Ss_sold_date_sk: 1,
			Ss_item_sk:      1,
		},
		Store_sale{
			Ss_customer_sk:  1,
			Ss_sold_date_sk: 1,
			Ss_item_sk:      1,
		},
		Store_sale{
			Ss_customer_sk:  1,
			Ss_sold_date_sk: 1,
			Ss_item_sk:      1,
		},
		Store_sale{
			Ss_customer_sk:  1,
			Ss_sold_date_sk: 1,
			Ss_item_sk:      1,
		},
		Store_sale{
			Ss_customer_sk:  1,
			Ss_sold_date_sk: 1,
			Ss_item_sk:      1,
		},
		Store_sale{
			Ss_customer_sk:  1,
			Ss_sold_date_sk: 1,
			Ss_item_sk:      1,
		},
		Store_sale{
			Ss_customer_sk:  1,
			Ss_sold_date_sk: 1,
			Ss_item_sk:      1,
		},
		Store_sale{
			Ss_customer_sk:  1,
			Ss_sold_date_sk: 1,
			Ss_item_sk:      1,
		},
	}
	date_dim = []Date_dim{Date_dim{
		D_date_sk:   1,
		D_year:      1999,
		D_moy:       5,
		D_month_seq: 120,
	}}
	item = []Item{Item{
		I_item_sk:       1,
		I_category:      "A",
		I_current_price: 100.0,
	}, Item{
		I_item_sk:       2,
		I_category:      "A",
		I_current_price: 50.0,
	}}
	target_month_seq = _maxOrdered[int](func() []int {
		results := []int{}
		for _, d := range date_dim {
			if (d.D_year == 1999) && (d.D_moy == 5) {
				if (d.D_year == 1999) && (d.D_moy == 5) {
					results = append(results, d.D_month_seq)
				}
			}
		}
		return results
	}())
	result = _convSlice[v, Result](func() []Result {
		groups := map[string]*data.Group{}
		order := []string{}
		for _, a := range customer_address {
			for _, c := range customer {
				if !(a.Ca_address_sk == c.C_current_addr_sk) {
					continue
				}
				for _, s := range store_sales {
					if !(c.C_customer_sk == s.Ss_customer_sk) {
						continue
					}
					for _, d := range date_dim {
						if !(s.Ss_sold_date_sk == d.D_date_sk) {
							continue
						}
						for _, i := range item {
							if !(s.Ss_item_sk == i.I_item_sk) {
								continue
							}
							if (d.D_month_seq == target_month_seq) && (i.I_current_price > (1.2 * _avgOrdered[float64](func() []float64 {
								results := []float64{}
								for _, j := range item {
									if j.I_category == i.I_category {
										if j.I_category == i.I_category {
											results = append(results, j.I_current_price)
										}
									}
								}
								return results
							}()))) {
								key := a.Ca_state
								ks := fmt.Sprint(key)
								g, ok := groups[ks]
								if !ok {
									g = &data.Group{Key: key}
									groups[ks] = g
									order = append(order, ks)
								}
								g.Items = append(g.Items, a)
							}
						}
					}
				}
			}
		}
		items := []*data.Group{}
		for _, ks := range order {
			items = append(items, groups[ks])
		}
		type pair struct {
			item *data.Group
			key  any
		}
		pairs := make([]pair, len(items))
		for idx, it := range items {
			g := it
			pairs[idx] = pair{item: it, key: _toAnySlice([]any{len(g.Items), g.Key.(any)})}
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
		for idx, p := range pairs {
			items[idx] = p.item
		}
		items = _paginate[*data.Group](items, -1, 100)
		results := []Result{}
		for _, g := range items {
			if !(len(g.Items) >= 10) {
				continue
			}
			results = append(results, Result{
				State: g.Key.(any),
				Cnt:   len(g.Items),
			})
		}
		return results
	}())
	func() { b, _ := json.Marshal(result); fmt.Println(string(b)) }()
	test_TPCDS_Q6_result()
}

func _avgOrdered[T constraints.Integer | constraints.Float](s []T) float64 {
	if len(s) == 0 {
		return 0
	}
	var sum float64
	for _, v := range s {
		sum += float64(v)
	}
	return sum / float64(len(s))
}

func _convSlice[T any, U any](s []T) []U {
	out := make([]U, len(s))
	for i, v := range s {
		out[i] = any(v).(U)
	}
	return out
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

func _maxOrdered[T constraints.Ordered](s []T) T {
	if len(s) == 0 {
		var zero T
		return zero
	}
	m := s[0]
	for _, v := range s[1:] {
		if v > m {
			m = v
		}
	}
	return m
}

func _paginate[T any](src []T, skip, take int) []T {
	if skip > 0 {
		if skip < len(src) {
			src = src[skip:]
		} else {
			return []T{}
		}
	}
	if take >= 0 && take < len(src) {
		src = src[:take]
	}
	return src
}

func _toAnySlice[T any](s []T) []any {
	out := make([]any, len(s))
	for i, v := range s {
		out[i] = v
	}
	return out
}
