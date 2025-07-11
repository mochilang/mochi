//go:build ignore

package main

import (
	"encoding/json"
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
		N_nationkey: 1,
		N_name:      "BRAZIL",
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
		C_custkey:   1,
		C_name:      "Alice",
		C_acctbal:   100.0,
		C_nationkey: 1,
		C_address:   "123 St",
		C_phone:     "123-456",
		C_comment:   "Loyal",
	}}
	type OrdersItem struct {
		O_orderkey  int    `json:"o_orderkey"`
		O_custkey   int    `json:"o_custkey"`
		O_orderdate string `json:"o_orderdate"`
	}

	var orders []OrdersItem = []OrdersItem{OrdersItem{
		O_orderkey:  1000,
		O_custkey:   1,
		O_orderdate: "1993-10-15",
	}, OrdersItem{
		O_orderkey:  2000,
		O_custkey:   1,
		O_orderdate: "1994-01-02",
	}}
	_ = orders
	type LineitemItem struct {
		L_orderkey      int     `json:"l_orderkey"`
		L_returnflag    string  `json:"l_returnflag"`
		L_extendedprice float64 `json:"l_extendedprice"`
		L_discount      float64 `json:"l_discount"`
	}

	var lineitem []LineitemItem = []LineitemItem{LineitemItem{
		L_orderkey:      1000,
		L_returnflag:    "R",
		L_extendedprice: 1000.0,
		L_discount:      0.1,
	}, LineitemItem{
		L_orderkey:      2000,
		L_returnflag:    "N",
		L_extendedprice: 500.0,
		L_discount:      0.0,
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

	type _ struct {
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
							key := _{
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
							for k, v := range _cast[map[string]any](c) {
								_item[k] = v
							}
							_item["c"] = c
							for k, v := range _cast[map[string]any](o) {
								_item[k] = v
							}
							_item["o"] = o
							for k, v := range _cast[map[string]any](l) {
								_item[k] = v
							}
							_item["l"] = l
							for k, v := range _cast[map[string]any](n) {
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
				_res := []any{}
				for _, x := range g.Items {
					_res = append(_res, (_cast[float64](_cast[map[string]any](_cast[map[string]any](x)["l"])["l_extendedprice"]) * _cast[float64]((_cast[float64](1) - _cast[float64](_cast[map[string]any](_cast[map[string]any](x)["l"])["l_discount"])))))
				}
				return _res
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
		_res := []Result{}
		for _, g := range items {
			_res = append(_res, Result{
				C_custkey: _cast[map[string]any](g.Key)["c_custkey"],
				C_name:    _cast[map[string]any](g.Key)["c_name"],
				Revenue: _sum(func() []any {
					_res := []any{}
					for _, x := range g.Items {
						_res = append(_res, (_cast[float64](_cast[map[string]any](_cast[map[string]any](x)["l"])["l_extendedprice"]) * _cast[float64]((_cast[float64](1) - _cast[float64](_cast[map[string]any](_cast[map[string]any](x)["l"])["l_discount"])))))
					}
					return _res
				}()),
				C_acctbal: _cast[map[string]any](g.Key)["c_acctbal"],
				N_name:    _cast[map[string]any](g.Key)["n_name"],
				C_address: _cast[map[string]any](g.Key)["c_address"],
				C_phone:   _cast[map[string]any](g.Key)["c_phone"],
				C_comment: _cast[map[string]any](g.Key)["c_comment"],
			})
		}
		return _res
	}()
	fmt.Println(strings.TrimSuffix(strings.TrimPrefix(_fmt(result), "["), "]"))
}

func _cast[T any](v any) T {
	if tv, ok := v.(T); ok {
		return tv
	}
	var out T
	switch any(out).(type) {
	case int:
		switch vv := v.(type) {
		case int:
			return any(vv).(T)
		case float64:
			return any(int(vv)).(T)
		case float32:
			return any(int(vv)).(T)
		}
	case float64:
		switch vv := v.(type) {
		case int:
			return any(float64(vv)).(T)
		case float64:
			return any(vv).(T)
		case float32:
			return any(float64(vv)).(T)
		}
	case float32:
		switch vv := v.(type) {
		case int:
			return any(float32(vv)).(T)
		case float64:
			return any(float32(vv)).(T)
		case float32:
			return any(vv).(T)
		}
	}
	if m, ok := v.(map[any]any); ok {
		v = _convertMapAny(m)
	}
	data, err := json.Marshal(v)
	if err != nil {
		panic(err)
	}
	if err := json.Unmarshal(data, &out); err != nil {
		panic(err)
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

func _fmt(v any) string {
	if v == nil {
		return "<nil>"
	}
	rv := reflect.ValueOf(v)
	if rv.Kind() == reflect.Pointer {
		if rv.IsNil() {
			return "<nil>"
		}
		v = rv.Elem().Interface()
		rv = reflect.ValueOf(v)
	}
	if rv.Kind() == reflect.Struct {
		if rv.IsZero() {
			return "<nil>"
		}
		b, _ := json.Marshal(v)
		var m map[string]any
		_ = json.Unmarshal(b, &m)
		return fmt.Sprint(m)
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
