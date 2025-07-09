//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
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
	var result []map[string]any = func() []map[string]any {
		_res := []map[string]any{}
		for _, c := range customers {
			matched := false
			for _, o := range orders {
				if !(o.CustomerId == c.Id) {
					continue
				}
				matched = true
				_res = append(_res, map[string]any{"customerName": c.Name, "order": o})
			}
			if !matched {
				var o OrdersItem
				_res = append(_res, map[string]any{"customerName": c.Name, "order": o})
			}
		}
		return _res
	}()
	fmt.Println("--- Right Join using syntax ---")
	for _, entry := range result {
		if entry["order"] {
			fmt.Println("Customer", entry["customerName"], "has order", _cast[map[string]any](entry["order"])["id"], "- $", _cast[map[string]any](entry["order"])["total"])
		} else {
			fmt.Println("Customer", entry["customerName"], "has no orders")
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
