//go:build ignore

// Generated by Mochi compiler v0.10.26 on 2025-07-16T01:05:51Z

package main

import (
	"encoding/json"
	"fmt"
	"reflect"
	"time"
)

type Result struct {
	Item_id int     `json:"item_id"`
	Average float64 `json:"average"`
}

type v = Result

func expect(cond bool) {
	if !cond {
		panic("expect failed")
	}
}

func formatDuration(d time.Duration) string {
	switch {
	case d < time.Microsecond:
		return fmt.Sprintf("%dns", d.Nanoseconds())
	case d < time.Millisecond:
		return fmt.Sprintf("%.1fµs", float64(d.Microseconds()))
	case d < time.Second:
		return fmt.Sprintf("%.1fms", float64(d.Milliseconds()))
	default:
		return fmt.Sprintf("%.2fs", d.Seconds())
	}
}

func printTestStart(name string) {
	fmt.Printf("   test %-30s ...", name)
}

func printTestPass(d time.Duration) {
	fmt.Printf(" ok (%s)\n", formatDuration(d))
}

func printTestFail(err error, d time.Duration) {
	fmt.Printf(" fail %v (%s)\n", err, formatDuration(d))
}

func test_TPCDS_Q58_simplified() {
	expect(_equal(result, []v{v{
		Item_id: 1,
		Average: 58.0,
	}}))
}

var ss_items v
var cs_items v
var ws_items v
var avg float64
var result []Result

func main() {
	ss_items = v{
		Item_id:     1,
		Ss_item_rev: 50.0,
	}
	cs_items = v{
		Item_id:     1,
		Cs_item_rev: 60.0,
	}
	ws_items = v{
		Item_id:     1,
		Ws_item_rev: 64.0,
	}
	avg = (((ss_items.Ss_item_rev + cs_items.Cs_item_rev) + ws_items.Ws_item_rev) / 3.0)
	result = []Result{Result{
		Item_id: ss_items.Item_id,
		Average: avg,
	}}
	func() { b, _ := json.Marshal(result); fmt.Println(string(b)) }()
	test_TPCDS_Q58_simplified()
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
