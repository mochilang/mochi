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
	type NationItem struct {
		N_nationkey int    `json:"n_nationkey"`
		N_name      string `json:"n_name"`
	}

	var nation []NationItem = []NationItem{NationItem{
		1,
		"BRAZIL",
	}}
	_ = nation
	type CustomerItem struct {
		C_custkey   int     `json:"c_custkey"`
		C_name      string  `json:"c_name"`
		C_acctbal   float64 `json:"c_acctbal"`
		C_nationkey int     `json:"c_nationkey"`
		C_address   string  `json:"c_address"`
		C_phone     string  `json:"c_phone"`
		C_comment   string  `json:"c_comment"`
	}

	var customer []CustomerItem = []CustomerItem{CustomerItem{
		1,
		"Alice",
		100.0,
		1,
		"123 St",
		"123-456",
		"Loyal",
	}}
	type OrdersItem struct {
		O_orderkey  int    `json:"o_orderkey"`
		O_custkey   int    `json:"o_custkey"`
		O_orderdate string `json:"o_orderdate"`
	}

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
	type LineitemItem struct {
		L_orderkey      int     `json:"l_orderkey"`
		L_returnflag    string  `json:"l_returnflag"`
		L_extendedprice float64 `json:"l_extendedprice"`
		L_discount      float64 `json:"l_discount"`
	}

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
	type Result struct {
		C_custkey any `json:"c_custkey"`
		C_name    any `json:"c_name"`
		Revenue   int `json:"revenue"`
		C_acctbal any `json:"c_acctbal"`
		N_name    any `json:"n_name"`
		C_address any `json:"c_address"`
		C_phone   any `json:"c_phone"`
		C_comment any `json:"c_comment"`
	}

	type v struct {
		C_custkey int     `json:"c_custkey"`
		C_name    string  `json:"c_name"`
		C_acctbal float64 `json:"c_acctbal"`
		C_address string  `json:"c_address"`
		C_phone   string  `json:"c_phone"`
		C_comment string  `json:"c_comment"`
		N_name    string  `json:"n_name"`
	}

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
								c.C_custkey,
								c.C_name,
								c.C_acctbal,
								c.C_address,
								c.C_phone,
								c.C_comment,
								n.N_name,
							}
							ks := fmt.Sprint(key)
							g, ok := groups[ks]
							if !ok {
								g = &data.Group{Key: key}
								groups[ks] = g
								order = append(order, ks)
							}
							_item := map[string]any{}
							for k, v := range _toAnyMap(c) {
								_item[k] = v
							}
							_item["c"] = c
							for k, v := range _toAnyMap(o) {
								_item[k] = v
							}
							_item["o"] = o
							for k, v := range _toAnyMap(l) {
								_item[k] = v
							}
							_item["l"] = l
							for k, v := range _toAnyMap(n) {
								_item[k] = v
							}
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
	fmt.Println(result)
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
