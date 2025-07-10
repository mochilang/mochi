//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
	"mochi/runtime/data"
	"reflect"
	"strconv"
	"strings"
)

func main() {
	var customers []map[string]any = []map[string]any{
		map[string]any{"id": 1, "name": "Alice"},
		map[string]any{"id": 2, "name": "Bob"},
		map[string]any{"id": 3, "name": "Charlie"},
		map[string]any{"id": 4, "name": "Diana"},
	}
	var orders []map[string]int = []map[string]int{map[string]int{
		"id":         100,
		"customerId": 1,
		"total":      250,
	}, map[string]int{
		"id":         101,
		"customerId": 2,
		"total":      125,
	}, map[string]int{
		"id":         102,
		"customerId": 1,
		"total":      300,
	}}
	_ = orders
	var result []map[string]any = func() []map[string]any {
		_res := []map[string]any{}
		for _, c := range customers {
			matched := false
			for _, o := range orders {
				if !(_equal(o["customerId"], c["id"])) {
					continue
				}
				matched = true
				_res = append(_res, map[string]any{"customerName": c["name"], "order": o})
			}
			if !matched {
				var o map[string]int
				_res = append(_res, map[string]any{"customerName": c["name"], "order": o})
			}
		}
		return _res
	}()
	fmt.Println("--- Right Join using syntax ---")
	for _, entry := range result {
		if _exists(entry["order"]) {
			fmt.Println(strings.TrimRight(strings.Join([]string{fmt.Sprint("Customer"), fmt.Sprint(entry["customerName"]), fmt.Sprint("has order"), fmt.Sprint(_cast[map[string]any](entry["order"])["id"]), fmt.Sprint("- $"), fmt.Sprint(_cast[map[string]any](entry["order"])["total"])}, " "), " "))
		} else {
			fmt.Println(strings.TrimRight(strings.Join([]string{fmt.Sprint("Customer"), fmt.Sprint(entry["customerName"]), fmt.Sprint("has no orders")}, " "), " "))
		}
	}
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

func _exists(v any) bool {
	if g, ok := v.(*data.Group); ok {
		return len(g.Items) > 0
	}
	switch s := v.(type) {
	case []any:
		return len(s) > 0
	case []int:
		return len(s) > 0
	case []float64:
		return len(s) > 0
	case []string:
		return len(s) > 0
	case []bool:
		return len(s) > 0
	case []map[string]any:
		return len(s) > 0
	case map[string]any:
		return len(s) > 0
	case string:
		return len([]rune(s)) > 0
	}
	rv := reflect.ValueOf(v)
	if rv.Kind() == reflect.Slice || rv.Kind() == reflect.Array {
		return rv.Len() > 0
	}
	return false
}
