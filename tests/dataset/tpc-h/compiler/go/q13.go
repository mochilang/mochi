//go:build ignore

// Generated by Mochi compiler v0.10.28 on 2025-07-18T06:59:52Z

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

type v map[string]any

type Customer struct {
	C_custkey int `json:"c_custkey"`
}

type Order struct {
	O_orderkey int    `json:"o_orderkey"`
	O_custkey  int    `json:"o_custkey"`
	O_comment  string `json:"o_comment"`
}

type Per_customer struct {
	C_count int `json:"c_count"`
}

type Grouped struct {
	C_count  any `json:"c_count"`
	Custdist int `json:"custdist"`
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

func test_Q13_groups_customers_by_non_special_order_count() {
	expect(_equal(grouped, []map[string]int{map[string]int{"c_count": 2, "custdist": 1}, map[string]int{"c_count": 0, "custdist": 2}}))
}

var customer []Customer
var orders []Order
var per_customer []Per_customer
var grouped []Grouped

func main() {
	customer = []Customer{Customer{C_custkey: 1}, Customer{C_custkey: 2}, Customer{C_custkey: 3}}
	orders = []Order{Order{
		O_orderkey: 100,
		O_custkey:  1,
		O_comment:  "fast delivery",
	}, Order{
		O_orderkey: 101,
		O_custkey:  1,
		O_comment:  "no comment",
	}, Order{
		O_orderkey: 102,
		O_custkey:  2,
		O_comment:  "special requests only",
	}}
	per_customer = _convSlice[map[string]int, Per_customer](func() []Per_customer {
		results := []Per_customer{}
		for _, c := range customer {
			results = append(results, Per_customer{C_count: len(func() []Order {
				results := []Order{}
				for _, o := range orders {
					if ((o.O_custkey == c.C_custkey) && (!(strings.Contains(o.O_comment, "special")))) && (!(strings.Contains(o.O_comment, "requests"))) {
						if ((o.O_custkey == c.C_custkey) && (!(strings.Contains(o.O_comment, "special")))) && (!(strings.Contains(o.O_comment, "requests"))) {
							results = append(results, o)
						}
					}
				}
				return results
			}())})
		}
		return results
	}())
	grouped = _convSlice[map[string]int, Grouped](func() []Grouped {
		groups := map[string]*data.Group{}
		order := []string{}
		for _, x := range per_customer {
			key := x.C_count
			ks := fmt.Sprint(key)
			g, ok := groups[ks]
			if !ok {
				g = &data.Group{Key: key}
				groups[ks] = g
				order = append(order, ks)
			}
			g.Items = append(g.Items, x)
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
			pairs[idx] = pair{item: it, key: -(g.Key.(any)).(float64)}
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
		results := []Grouped{}
		for _, g := range items {
			results = append(results, Grouped{
				C_count:  g.Key.(any),
				Custdist: len(g.Items),
			})
		}
		return results
	}())
	func() { b, _ := json.Marshal(any(grouped)); fmt.Println(string(b)) }()
	test_Q13_groups_customers_by_non_special_order_count()
}

func _convSlice[T any, U any](s []T) []U {
	out := make([]U, len(s))
	for i, v := range s {
		out[i] = any(v).(U)
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
