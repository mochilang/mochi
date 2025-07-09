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
	type CustomersItem struct {
		Id   int    `json:"id"`
		Name string `json:"name"`
	}

	var customers []CustomersItem = []CustomersItem{
		CustomersItem{
			Id:   1,
			Name: "Alice",
		},
		CustomersItem{
			Id:   2,
			Name: "Bob",
		},
		CustomersItem{
			Id:   3,
			Name: "Charlie",
		},
		CustomersItem{
			Id:   4,
			Name: "Diana",
		},
	}
	type OrdersItem struct {
		Id         int `json:"id"`
		CustomerId int `json:"customerId"`
		Total      int `json:"total"`
	}

	var orders []OrdersItem = []OrdersItem{OrdersItem{
		Id:         100,
		CustomerId: 1,
		Total:      250,
	}, OrdersItem{
		Id:         101,
		CustomerId: 2,
		Total:      125,
	}, OrdersItem{
		Id:         102,
		CustomerId: 1,
		Total:      300,
	}}
	_ = orders
	type Result struct {
		CustomerName any `json:"customerName"`
		Order        any `json:"order"`
	}

	type Result1 struct {
		CustomerName string     `json:"customerName"`
		Order        OrdersItem `json:"order"`
	}

	var result []Result = _cast[[]Result](func() []Result1 {
		_res := []Result1{}
		for _, c := range customers {
			matched := false
			for _, o := range orders {
				if !(o.CustomerId == c.Id) {
					continue
				}
				matched = true
				_res = append(_res, Result1{
					CustomerName: c.Name,
					Order:        o,
				})
			}
			if !matched {
				var o OrdersItem
				_res = append(_res, Result1{
					CustomerName: c.Name,
					Order:        o,
				})
			}
		}
		return _res
	}())
	fmt.Println("--- Right Join using syntax ---")
	for _, entry := range result {
		if _exists(entry.Order) {
			fmt.Println(strings.TrimRight(strings.Join([]string{fmt.Sprint("Customer"), fmt.Sprint(entry.CustomerName), fmt.Sprint("has order"), fmt.Sprint(_cast[map[string]any](entry.Order)["id"]), fmt.Sprint("- $"), fmt.Sprint(_cast[map[string]any](entry.Order)["total"])}, " "), " "))
		} else {
			fmt.Println(strings.TrimRight(strings.Join([]string{fmt.Sprint("Customer"), fmt.Sprint(entry.CustomerName), fmt.Sprint("has no orders")}, " "), " "))
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
