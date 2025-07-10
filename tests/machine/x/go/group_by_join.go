//go:build ignore

package main

import (
	"fmt"
	"mochi/runtime/data"
	"reflect"
	"strings"
)

func main() {
	var customers []map[string]any = []map[string]any{map[string]any{"id": 1, "name": "Alice"}, map[string]any{"id": 2, "name": "Bob"}}
	_ = customers
	var orders []map[string]int = []map[string]int{map[string]int{"id": 100, "customerId": 1}, map[string]int{"id": 101, "customerId": 1}, map[string]int{"id": 102, "customerId": 2}}
	var stats []map[string]any = func() []map[string]any {
		groups := map[string]*data.Group{}
		order := []string{}
		for _, o := range orders {
			for _, c := range customers {
				if !(_equal(o["customerId"], c["id"])) {
					continue
				}
				key := c["name"]
				ks := fmt.Sprint(key)
				g, ok := groups[ks]
				if !ok {
					g = &data.Group{Key: key}
					groups[ks] = g
					order = append(order, ks)
				}
				_item := map[string]any{}
				for k, v := range _cast[map[string]any](o) {
					_item[k] = v
				}
				_item["o"] = o
				for k, v := range _cast[map[string]any](c) {
					_item[k] = v
				}
				_item["c"] = c
				g.Items = append(g.Items, _item)
			}
		}
		items := []*data.Group{}
		for _, ks := range order {
			items = append(items, groups[ks])
		}
		_res := []map[string]any{}
		for _, g := range items {
			_res = append(_res, map[string]any{"name": g.Key, "count": len(g.Items)})
		}
		return _res
	}()
	fmt.Println("--- Orders per customer ---")
	for _, s := range stats {
		fmt.Println(strings.TrimRight(strings.Join([]string{fmt.Sprint(s["name"]), fmt.Sprint("orders:"), fmt.Sprint(s["count"])}, " "), " "))
	}
}

func _cast[T any](v any) T {
	return v.(T)
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
