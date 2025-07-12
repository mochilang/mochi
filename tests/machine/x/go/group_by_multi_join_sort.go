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
	var nation []NationItem = []NationItem{NationItem{
		1,
		"BRAZIL",
	}}
	_ = nation
	var customer []CustomerItem = []CustomerItem{CustomerItem{
		1,
		"Alice",
		100.0,
		1,
		"123 St",
		"123-456",
		"Loyal",
	}}
	var orders []OrdersItem = []OrdersItem{OrdersItem{
		1000,
		1,
		"1993-10-15",
	}, OrdersItem{
		2000,
		1,
		"1994-01-02",
	}}
	_ = orders
	var lineitem []LineitemItem = []LineitemItem{LineitemItem{
		1000,
		"R",
		1000.0,
		0.1,
	}, LineitemItem{
		2000,
		"N",
		500.0,
		0.0,
	}}
	_ = lineitem
	var start_date string = "1993-10-01"
	var end_date string = "1994-01-01"
	var result []Result = func() []Result {
		groups := map[string]*data.Group{}
		order := []string{}
		for _, c := range customer {
			for _, o := range orders {
				if !(o.O_custkey == c.C_custkey) {
					continue
				}
				for _, l := range lineitem {
					if !(l.L_orderkey == o.O_orderkey) {
						continue
					}
					for _, n := range nation {
						if !(n.N_nationkey == c.C_nationkey) {
							continue
						}
						if ((o.O_orderdate >= start_date) && (o.O_orderdate < end_date)) && (l.L_returnflag == "R") {
							key := v{
								C_custkey: c.C_custkey,
								C_name:    c.C_name,
								C_acctbal: c.C_acctbal,
								C_address: c.C_address,
								C_phone:   c.C_phone,
								C_comment: c.C_comment,
								N_name:    n.N_name,
							}
							ks := fmt.Sprint(key)
							g, ok := groups[ks]
							if !ok {
								g = &data.Group{Key: key}
								groups[ks] = g
								order = append(order, ks)
							}
							_item := map[string]any{}
							_item["c_custkey"] = c.C_custkey
							_item["c_name"] = c.C_name
							_item["c_acctbal"] = c.C_acctbal
							_item["c_nationkey"] = c.C_nationkey
							_item["c_address"] = c.C_address
							_item["c_phone"] = c.C_phone
							_item["c_comment"] = c.C_comment
							_item["c"] = c
							_item["o_orderkey"] = o.O_orderkey
							_item["o_custkey"] = o.O_custkey
							_item["o_orderdate"] = o.O_orderdate
							_item["o"] = o
							_item["l_orderkey"] = l.L_orderkey
							_item["l_returnflag"] = l.L_returnflag
							_item["l_extendedprice"] = l.L_extendedprice
							_item["l_discount"] = l.L_discount
							_item["l"] = l
							_item["n_nationkey"] = n.N_nationkey
							_item["n_name"] = n.N_name
							_item["n"] = n
							g.Items = append(g.Items, _item)
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
			pairs[idx] = pair{item: it, key: -_sum(func() []any {
				results := []any{}
				for _, x := range g.Items {
					results = append(results, ((_toAnyMap(_toAnyMap(x)["l"])["l_extendedprice"]).(float64) * (float64(1) - (_toAnyMap(_toAnyMap(x)["l"])["l_discount"]).(float64)).(float64)))
				}
				return results
			}())}
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
				_toAnyMap(g.Key)["c_custkey"],
				_toAnyMap(g.Key)["c_name"],
				_sum(func() []any {
					results := []any{}
					for _, x := range g.Items {
						results = append(results, ((_toAnyMap(_toAnyMap(x)["l"])["l_extendedprice"]).(float64) * (float64(1) - (_toAnyMap(_toAnyMap(x)["l"])["l_discount"]).(float64)).(float64)))
					}
					return results
				}()),
				_toAnyMap(g.Key)["c_acctbal"],
				_toAnyMap(g.Key)["n_name"],
				_toAnyMap(g.Key)["c_address"],
				_toAnyMap(g.Key)["c_phone"],
				_toAnyMap(g.Key)["c_comment"],
			})
		}
		return results
	}()
	_print(result)
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

func _sum(v any) float64 {
	var items []any
	if g, ok := v.(*data.Group); ok {
		items = g.Items
	} else {
		switch s := v.(type) {
		case []any:
			items = s
		case []int:
			items = []any{}
			for _, v := range s {
				items = append(items, v)
			}
		case []float64:
			items = []any{}
			for _, v := range s {
				items = append(items, v)
			}
		case []string, []bool:
			panic("sum() expects numbers")
		default:
			panic("sum() expects list or group")
		}
	}
	var sum float64
	for _, it := range items {
		switch n := it.(type) {
		case int:
			sum += float64(n)
		case int64:
			sum += float64(n)
		case float64:
			sum += n
		default:
			panic("sum() expects numbers")
		}
	}
	return sum
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
