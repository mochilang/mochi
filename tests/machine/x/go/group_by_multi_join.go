//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
	"mochi/runtime/data"
	"reflect"
	"strings"
)

func main() {
	var nations []map[string]any = []map[string]any{map[string]any{"id": 1, "name": "A"}, map[string]any{"id": 2, "name": "B"}}
	_ = nations
	var suppliers []map[string]int = []map[string]int{map[string]int{"id": 1, "nation": 1}, map[string]int{"id": 2, "nation": 2}}
	_ = suppliers
	var partsupp []map[string]any = []map[string]any{map[string]any{
		"part":     100,
		"supplier": 1,
		"cost":     10.0,
		"qty":      2,
	}, map[string]any{
		"part":     100,
		"supplier": 2,
		"cost":     20.0,
		"qty":      1,
	}, map[string]any{
		"part":     200,
		"supplier": 1,
		"cost":     5.0,
		"qty":      3,
	}}
	var filtered []map[string]any = func() []map[string]any {
		_res := []map[string]any{}
		for _, ps := range partsupp {
			for _, s := range suppliers {
				if !(_equal(s["id"], ps["supplier"])) {
					continue
				}
				for _, n := range nations {
					if !(_equal(n["id"], s["nation"])) {
						continue
					}
					if _equal(n["name"], "A") {
						if _equal(n["name"], "A") {
							_res = append(_res, map[string]any{"part": ps["part"], "value": (_cast[float64](ps["cost"]) * _cast[float64](ps["qty"]))})
						}
					}
				}
			}
		}
		return _res
	}()
	var grouped []map[string]any = func() []map[string]any {
		groups := map[string]*data.Group{}
		order := []string{}
		for _, x := range filtered {
			key := x["part"]
			ks := fmt.Sprint(key)
			g, ok := groups[ks]
			if !ok {
				g = &data.Group{Key: key}
				groups[ks] = g
				order = append(order, ks)
			}
			g.Items = append(g.Items, x)
		}
		_res := []map[string]any{}
		for _, ks := range order {
			g := groups[ks]
			_res = append(_res, map[string]any{"part": g.Key, "total": _sum(func() []any {
				_res := []any{}
				for _, r := range g.Items {
					_res = append(_res, _cast[map[string]any](r)["value"])
				}
				return _res
			}())})
		}
		return _res
	}()
	fmt.Println(strings.TrimSuffix(strings.TrimPrefix(fmt.Sprint(grouped), "["), "]"))
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
