//go:build ignore

package main

import (
	"fmt"
	"reflect"
	"strings"
)

func main() {
	var customers []map[string]any = []map[string]any{map[string]any{"id": 1, "name": "Alice"}, map[string]any{"id": 2, "name": "Bob"}}
	_ = customers
	var orders []map[string]int = []map[string]int{map[string]int{"id": 100, "customerId": 1}, map[string]int{"id": 101, "customerId": 2}}
	var items []map[string]any = []map[string]any{map[string]any{"orderId": 100, "sku": "a"}, map[string]any{"orderId": 101, "sku": "b"}}
	_ = items
	var result []map[string]any = func() []map[string]any {
		_res := []map[string]any{}
		for _, o := range orders {
			for _, c := range customers {
				if !(_equal(o["customerId"], c["id"])) {
					continue
				}
				for _, i := range items {
					if !(_equal(o["id"], i["orderId"])) {
						continue
					}
					_res = append(_res, map[string]any{"name": c["name"], "sku": i["sku"]})
				}
			}
		}
		return _res
	}()
	fmt.Println("--- Multi Join ---")
	for _, r := range result {
		fmt.Println(strings.TrimRight(strings.Join([]string{fmt.Sprint(r["name"]), fmt.Sprint("bought item"), fmt.Sprint(r["sku"])}, " "), " "))
	}
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
