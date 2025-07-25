//go:build ignore

// Generated by Mochi compiler v0.10.28 on 2025-07-18T06:59:40Z

package main

import (
	"encoding/json"
	"fmt"
	"mochi/runtime/data"
	"reflect"
	"strings"
	"time"

	"golang.org/x/exp/constraints"
)

type v = Result

type Lineitem struct {
	L_quantity      int     `json:"l_quantity"`
	L_extendedprice float64 `json:"l_extendedprice"`
	L_discount      float64 `json:"l_discount"`
	L_tax           float64 `json:"l_tax"`
	L_returnflag    string  `json:"l_returnflag"`
	L_linestatus    string  `json:"l_linestatus"`
	L_shipdate      string  `json:"l_shipdate"`
}

type Result struct {
	Returnflag     any     `json:"returnflag"`
	Linestatus     any     `json:"linestatus"`
	Sum_qty        float64 `json:"sum_qty"`
	Sum_base_price float64 `json:"sum_base_price"`
	Sum_disc_price float64 `json:"sum_disc_price"`
	Sum_charge     float64 `json:"sum_charge"`
	Avg_qty        float64 `json:"avg_qty"`
	Avg_price      float64 `json:"avg_price"`
	Avg_disc       float64 `json:"avg_disc"`
	Count_order    int     `json:"count_order"`
}

type GKey struct {
	Returnflag string `json:"returnflag"`
	Linestatus string `json:"linestatus"`
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

func test_Q1_aggregates_revenue_and_quantity_by_returnflag___linestatus() {
	expect(_equal(result, []map[string]any{map[string]any{
		"returnflag":     any("N"),
		"linestatus":     any("O"),
		"sum_qty":        any(53),
		"sum_base_price": any(3000),
		"sum_disc_price": any((950.0 + 1800.0)),
		"sum_charge":     any(((950.0 * 1.07) + (1800.0 * 1.05))),
		"avg_qty":        any(26.5),
		"avg_price":      any(1500),
		"avg_disc":       any(0.07500000000000001),
		"count_order":    any(2),
	}}))
}

var lineitem []Lineitem
var result []Result

func main() {
	lineitem = []Lineitem{Lineitem{
		L_quantity:      17,
		L_extendedprice: 1000.0,
		L_discount:      0.05,
		L_tax:           0.07,
		L_returnflag:    "N",
		L_linestatus:    "O",
		L_shipdate:      "1998-08-01",
	}, Lineitem{
		L_quantity:      36,
		L_extendedprice: 2000.0,
		L_discount:      0.1,
		L_tax:           0.05,
		L_returnflag:    "N",
		L_linestatus:    "O",
		L_shipdate:      "1998-09-01",
	}, Lineitem{
		L_quantity:      25,
		L_extendedprice: 1500.0,
		L_discount:      0.0,
		L_tax:           0.08,
		L_returnflag:    "R",
		L_linestatus:    "F",
		L_shipdate:      "1998-09-03",
	}}
	result = func() []Result {
		groups := map[string]*data.Group{}
		order := []string{}
		for _, row := range lineitem {
			if row.L_shipdate <= "1998-09-02" {
				key := GKey{
					Returnflag: row.L_returnflag,
					Linestatus: row.L_linestatus,
				}
				ks := fmt.Sprint(key)
				g, ok := groups[ks]
				if !ok {
					g = &data.Group{Key: key}
					groups[ks] = g
					order = append(order, ks)
				}
				g.Items = append(g.Items, row)
			}
		}
		items := []*data.Group{}
		for _, ks := range order {
			items = append(items, groups[ks])
		}
		results := []Result{}
		for _, g := range items {
			results = append(results, Result{
				Returnflag: any(g.Key.(GKey).Returnflag),
				Linestatus: any(g.Key.(GKey).Linestatus),
				Sum_qty: _sumOrdered[int](func() []int {
					results := []int{}
					for _, xRaw := range g.Items {
						x := xRaw.(Lineitem)
						results = append(results, x.L_quantity)
					}
					return results
				}()),
				Sum_base_price: _sumOrdered[float64](func() []float64 {
					results := []float64{}
					for _, xRaw := range g.Items {
						x := xRaw.(Lineitem)
						results = append(results, x.L_extendedprice)
					}
					return results
				}()),
				Sum_disc_price: _sumOrdered[float64](func() []float64 {
					results := []float64{}
					for _, xRaw := range g.Items {
						x := xRaw.(Lineitem)
						results = append(results, (x.L_extendedprice * (float64(1) - x.L_discount)))
					}
					return results
				}()),
				Sum_charge: _sumOrdered[float64](func() []float64 {
					results := []float64{}
					for _, xRaw := range g.Items {
						x := xRaw.(Lineitem)
						results = append(results, ((x.L_extendedprice * (float64(1) - x.L_discount)) * (float64(1) + x.L_tax)))
					}
					return results
				}()),
				Avg_qty: _avgOrdered[int](func() []int {
					results := []int{}
					for _, xRaw := range g.Items {
						x := xRaw.(Lineitem)
						results = append(results, x.L_quantity)
					}
					return results
				}()),
				Avg_price: _avgOrdered[float64](func() []float64 {
					results := []float64{}
					for _, xRaw := range g.Items {
						x := xRaw.(Lineitem)
						results = append(results, x.L_extendedprice)
					}
					return results
				}()),
				Avg_disc: _avgOrdered[float64](func() []float64 {
					results := []float64{}
					for _, xRaw := range g.Items {
						x := xRaw.(Lineitem)
						results = append(results, x.L_discount)
					}
					return results
				}()),
				Count_order: len(g.Items),
			})
		}
		return results
	}()
	func() { b, _ := json.Marshal(any(result)); fmt.Println(string(b)) }()
	test_Q1_aggregates_revenue_and_quantity_by_returnflag___linestatus()
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

func _sumOrdered[T constraints.Integer | constraints.Float](s []T) float64 {
	var sum float64
	for _, v := range s {
		sum += float64(v)
	}
	return sum
}
