//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
	"reflect"
	"strings"
)

func main() {
	fmt.Println(strings.TrimSuffix(strings.TrimPrefix(_fmt(_union[int]([]int{1, 2}, []int{2, 3})), "["), "]"))
	fmt.Println(strings.TrimSuffix(strings.TrimPrefix(_fmt(_except[int]([]int{1, 2, 3}, []int{2})), "["), "]"))
	fmt.Println(strings.TrimSuffix(strings.TrimPrefix(_fmt(_intersect[int]([]int{1, 2, 3}, []int{2, 4})), "["), "]"))
	fmt.Println(_fmt(len(append(append([]int{}, []int{1, 2}...), []int{2, 3}...))))
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

func _except[T any](a, b []T) []T {
	res := []T{}
	for _, x := range a {
		keep := true
		for _, y := range b {
			if _equal(x, y) {
				keep = false
				break
			}
		}
		if keep {
			res = append(res, x)
		}
	}
	return res
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

func _intersect[T any](a, b []T) []T {
	res := []T{}
	for _, x := range a {
		inB := false
		for _, y := range b {
			if _equal(x, y) {
				inB = true
				break
			}
		}
		if inB {
			exists := false
			for _, r := range res {
				if _equal(x, r) {
					exists = true
					break
				}
			}
			if !exists {
				res = append(res, x)
			}
		}
	}
	return res
}

func _union[T any](a, b []T) []T {
	res := append([]T{}, a...)
	for _, it := range b {
		found := false
		for _, v := range res {
			if _equal(v, it) {
				found = true
				break
			}
		}
		if !found {
			res = append(res, it)
		}
	}
	return res
}
