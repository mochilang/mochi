//go:build ignore

// Generated by Mochi compiler v0.10.26 on 2025-07-16T01:05:52Z

package main

import (
	"encoding/json"
	"fmt"
	"reflect"
	"strings"
	"time"

	"golang.org/x/exp/constraints"
)

type Catalog_sale struct {
	Item  int `json:"item"`
	Price int `json:"price"`
}

type Store_sale struct {
	Item  int `json:"item"`
	Price int `json:"price"`
}

type Web_sale struct {
	Item  int `json:"item"`
	Price int `json:"price"`
}

type v map[string]any

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

func test_TPCDS_Q60_simplified() {
	expect((result == 60))
}

var store_sales []Store_sale
var catalog_sales []Catalog_sale
var web_sales []Web_sale
var all_sales []any
var result float64

func main() {
	store_sales = []Store_sale{Store_sale{
		Item:  1,
		Price: 10,
	}, Store_sale{
		Item:  1,
		Price: 20,
	}}
	catalog_sales = []Catalog_sale{Catalog_sale{
		Item:  1,
		Price: 15,
	}}
	web_sales = []Web_sale{Web_sale{
		Item:  1,
		Price: 15,
	}}
	all_sales = append(append([]any{}, append(append([]any{}, _toAnySlice(store_sales)...), _toAnySlice(catalog_sales)...)...), _toAnySlice(web_sales)...)
	result = _sum(func() []any {
		results := []any{}
		for _, s := range all_sales {
			results = append(results, _getField(s, "price"))
		}
		return results
	}())
	func() { b, _ := json.Marshal(result); fmt.Println(string(b)) }()
	test_TPCDS_Q60_simplified()
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

func _getField(v any, name string) any {
	switch m := v.(type) {
	case map[string]any:
		return m[name]
	case map[string]string:
		if s, ok := m[name]; ok {
			return s
		}
	case map[any]any:
		return _convertMapAny(m)[name]
	default:
		rv := reflect.ValueOf(m)
		if rv.Kind() == reflect.Struct {
			rt := rv.Type()
			for i := 0; i < rv.NumField(); i++ {
				fn := rt.Field(i)
				field := fn.Name
				if tag := fn.Tag.Get("json"); tag != "" {
					if c := strings.Index(tag, ","); c >= 0 {
						tag = tag[:c]
					}
					if tag != "-" {
						field = tag
					}
				}
				if field == name {
					return rv.Field(i).Interface()
				}
			}
		}
	}
	return nil
}

func _sum[T constraints.Integer | constraints.Float](v []T) float64 {
	var sum float64
	for _, n := range v {
		sum += float64(n)
	}
	return sum
}

func _toAnySlice[T any](s []T) []any {
	out := make([]any, len(s))
	for i, v := range s {
		out[i] = v
	}
	return out
}
