//go:build ignore

// Generated by Mochi compiler v0.10.28 on 2025-07-18T06:59:55Z

package main

import (
	"encoding/json"
	"fmt"
	"mochi/runtime/data"
	"reflect"
	"sort"
	"strings"
	"time"
)

type v = Result

type Nation struct {
	N_nationkey int    `json:"n_nationkey"`
	N_name      string `json:"n_name"`
}

type Supplier struct {
	S_suppkey   int    `json:"s_suppkey"`
	S_name      string `json:"s_name"`
	S_nationkey int    `json:"s_nationkey"`
}

type Order struct {
	O_orderkey    int    `json:"o_orderkey"`
	O_orderstatus string `json:"o_orderstatus"`
}

type Lineitem struct {
	L_orderkey    int    `json:"l_orderkey"`
	L_suppkey     int    `json:"l_suppkey"`
	L_receiptdate string `json:"l_receiptdate"`
	L_commitdate  string `json:"l_commitdate"`
}

type Result struct {
	S_name  any `json:"s_name"`
	Numwait int `json:"numwait"`
}

type GRow struct {
	S  Supplier `json:"s"`
	L1 Lineitem `json:"l1"`
	O  Order    `json:"o"`
	N  Nation   `json:"n"`
}

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

func test_Q21_returns_Saudi_suppliers_who_caused_unique_delivery_delays() {
	expect(_equal(result, []map[string]any{map[string]any{"s_name": any("Desert Trade"), "numwait": any(1)}}))
}

var nation []Nation
var supplier []Supplier
var orders []Order
var lineitem []Lineitem
var result []Result

func main() {
	nation = []Nation{Nation{
		N_nationkey: 1,
		N_name:      "SAUDI ARABIA",
	}, Nation{
		N_nationkey: 2,
		N_name:      "FRANCE",
	}}
	supplier = []Supplier{Supplier{
		S_suppkey:   100,
		S_name:      "Desert Trade",
		S_nationkey: 1,
	}, Supplier{
		S_suppkey:   200,
		S_name:      "Euro Goods",
		S_nationkey: 2,
	}}
	orders = []Order{Order{
		O_orderkey:    500,
		O_orderstatus: "F",
	}, Order{
		O_orderkey:    600,
		O_orderstatus: "O",
	}}
	lineitem = []Lineitem{Lineitem{
		L_orderkey:    500,
		L_suppkey:     100,
		L_receiptdate: "1995-04-15",
		L_commitdate:  "1995-04-10",
	}, Lineitem{
		L_orderkey:    500,
		L_suppkey:     200,
		L_receiptdate: "1995-04-12",
		L_commitdate:  "1995-04-12",
	}, Lineitem{
		L_orderkey:    600,
		L_suppkey:     100,
		L_receiptdate: "1995-05-01",
		L_commitdate:  "1995-04-25",
	}}
	result = func() []Result {
		groups := map[string]*data.Group{}
		order := []string{}
		for _, s := range supplier {
			for _, l1 := range lineitem {
				if !(s.S_suppkey == l1.L_suppkey) {
					continue
				}
				for _, o := range orders {
					if !(o.O_orderkey == l1.L_orderkey) {
						continue
					}
					for _, n := range nation {
						if !(n.N_nationkey == s.S_nationkey) {
							continue
						}
						if (((o.O_orderstatus == "F") && (l1.L_receiptdate > l1.L_commitdate)) && (n.N_name == "SAUDI ARABIA")) && (!(len(func() []Lineitem {
							results := []Lineitem{}
							for _, x := range lineitem {
								if ((x.L_orderkey == l1.L_orderkey) && (x.L_suppkey != l1.L_suppkey)) && (x.L_receiptdate > x.L_commitdate) {
									if ((x.L_orderkey == l1.L_orderkey) && (x.L_suppkey != l1.L_suppkey)) && (x.L_receiptdate > x.L_commitdate) {
										results = append(results, x)
									}
								}
							}
							return results
						}()) > 0)) {
							key := s.S_name
							ks := fmt.Sprint(key)
							g, ok := groups[ks]
							if !ok {
								g = &data.Group{Key: key}
								groups[ks] = g
								order = append(order, ks)
							}
							g.Items = append(g.Items, GRow{S: s, L1: l1, O: o, N: n})
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
			pairs[idx] = pair{item: it, key: _toAnySlice([]any{-len(g.Items), g.Key.(any)})}
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
		results := []Result{}
		for _, g := range items {
			results = append(results, Result{
				S_name:  g.Key.(any),
				Numwait: len(g.Items),
			})
		}
		return results
	}()
	func() { b, _ := json.Marshal(any(result)); fmt.Println(string(b)) }()
	test_Q21_returns_Saudi_suppliers_who_caused_unique_delivery_delays()
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

func _copyToMap(dst map[string]any, src any) {
	switch m := src.(type) {
	case map[string]any:
		for k, v := range m {
			dst[k] = v
		}
	case map[string]string:
		for k, v := range m {
			dst[k] = v
		}
	case map[any]any:
		for k, v := range _convertMapAny(m) {
			dst[k] = v
		}
	default:
		rv := reflect.ValueOf(m)
		if rv.Kind() == reflect.Struct {
			rt := rv.Type()
			for i := 0; i < rv.NumField(); i++ {
				name := rt.Field(i).Name
				if tag := rt.Field(i).Tag.Get("json"); tag != "" {
					if c := strings.Index(tag, ","); c >= 0 {
						tag = tag[:c]
					}
					if tag != "-" {
						name = tag
					}
				}
				dst[name] = rv.Field(i).Interface()
			}
		}
	}
}

func _equal(a, b any) bool {
	av := reflect.ValueOf(a)
	bv := reflect.ValueOf(b)
	if av.Kind() == reflect.Struct && bv.Kind() == reflect.Map {
		am := map[string]any{}
		_copyToMap(am, a)
		bm := map[string]any{}
		_copyToMap(bm, b)
		return _equal(am, bm)
	}
	if av.Kind() == reflect.Map && bv.Kind() == reflect.Struct {
		am := map[string]any{}
		_copyToMap(am, a)
		bm := map[string]any{}
		_copyToMap(bm, b)
		return _equal(am, bm)
	}
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

func _toAnySlice[T any](s []T) []any {
	out := make([]any, len(s))
	for i, v := range s {
		out[i] = v
	}
	return out
}
