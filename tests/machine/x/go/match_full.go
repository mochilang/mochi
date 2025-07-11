//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

// line 32
func classify(n int) string {
	return func() string {
		_t := n
		if _equal(_t, 0) {
			return "zero"
		}
		if _equal(_t, 1) {
			return "one"
		}
		return "many"
	}()
}

func main() {
	var x int = 2
	var label string = func() string {
		_t := x
		if _equal(_t, 1) {
			return "one"
		}
		if _equal(_t, 2) {
			return "two"
		}
		if _equal(_t, 3) {
			return "three"
		}
		return "unknown"
	}()
	fmt.Println(_sprint(label))
	var day string = "sun"
	var mood string = func() string {
		_t := day
		if _equal(_t, "mon") {
			return "tired"
		}
		if _equal(_t, "fri") {
			return "excited"
		}
		if _equal(_t, "sun") {
			return "relaxed"
		}
		return "normal"
	}()
	fmt.Println(_sprint(mood))
	var ok bool = true
	var status string = func() string {
		_t := ok
		if _equal(_t, true) {
			return "confirmed"
		}
		if _equal(_t, false) {
			return "denied"
		}
		var _zero string
		return _zero
	}()
	fmt.Println(_sprint(status))
	fmt.Println(_sprint(classify(0)))
	fmt.Println(_sprint(classify(5)))
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
