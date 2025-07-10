//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
	"mochi/runtime/data"
	"reflect"
	"sort"
	"strconv"
	"strings"
)

func main() {
	var nation []map[string]any = []map[string]any{map[string]any{"n_nationkey": 1, "n_name": "BRAZIL"}}
	_ = nation
	var customer []map[string]any = []map[string]any{map[string]any{
		"c_custkey":   1,
		"c_name":      "Alice",
		"c_acctbal":   100.0,
		"c_nationkey": 1,
		"c_address":   "123 St",
		"c_phone":     "123-456",
		"c_comment":   "Loyal",
	}}
	var orders []map[string]any = []map[string]any{map[string]any{
		"o_orderkey":  1000,
		"o_custkey":   1,
		"o_orderdate": "1993-10-15",
	}, map[string]any{
		"o_orderkey":  2000,
		"o_custkey":   1,
		"o_orderdate": "1994-01-02",
	}}
	_ = orders
	var lineitem []map[string]any = []map[string]any{map[string]any{
		"l_orderkey":      1000,
		"l_returnflag":    "R",
		"l_extendedprice": 1000.0,
		"l_discount":      0.1,
	}, map[string]any{
		"l_orderkey":      2000,
		"l_returnflag":    "N",
		"l_extendedprice": 500.0,
		"l_discount":      0.0,
	}}
	_ = lineitem
	var start_date string = "1993-10-01"
	var end_date string = "1994-01-01"
	var result []map[string]any = func() []map[string]any {
		groups := map[string]*data.Group{}
		order := []string{}
		for _, c := range customer {
			for _, o := range orders {
				if !(_equal(o["o_custkey"], c["c_custkey"])) {
					continue
				}
				for _, l := range lineitem {
					if !(_equal(l["l_orderkey"], o["o_orderkey"])) {
						continue
					}
					for _, n := range nation {
						if !(_equal(n["n_nationkey"], c["c_nationkey"])) {
							continue
						}
						if ((_cast[string](o["o_orderdate"]) >= start_date) && (_cast[string](o["o_orderdate"]) < end_date)) && _equal(l["l_returnflag"], "R") {
							key := map[string]any{
								"c_custkey": c["c_custkey"],
								"c_name":    c["c_name"],
								"c_acctbal": c["c_acctbal"],
								"c_address": c["c_address"],
								"c_phone":   c["c_phone"],
								"c_comment": c["c_comment"],
								"n_name":    n["n_name"],
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
		_res := []map[string]any{}
		for _, g := range items {
			_res = append(_res, map[string]any{
				"c_custkey": _cast[map[string]any](g.Key)["c_custkey"],
				"c_name":    _cast[map[string]any](g.Key)["c_name"],
				"revenue": _sum(func() []any {
					_res := []any{}
					for _, x := range g.Items {
						_res = append(_res, (_cast[float64](_cast[map[string]any](_cast[map[string]any](x)["l"])["l_extendedprice"]) * _cast[float64]((_cast[float64](1) - _cast[float64](_cast[map[string]any](_cast[map[string]any](x)["l"])["l_discount"])))))
					}
					return _res
				}()),
				"c_acctbal": _cast[map[string]any](g.Key)["c_acctbal"],
				"n_name":    _cast[map[string]any](g.Key)["n_name"],
				"c_address": _cast[map[string]any](g.Key)["c_address"],
				"c_phone":   _cast[map[string]any](g.Key)["c_phone"],
				"c_comment": _cast[map[string]any](g.Key)["c_comment"],
			})
		}
		return _res
	}()
	fmt.Println(strings.TrimSuffix(strings.TrimPrefix(fmt.Sprint(result), "["), "]"))
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
		case string:
			n, _ := strconv.Atoi(vv)
			return any(n).(T)
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
