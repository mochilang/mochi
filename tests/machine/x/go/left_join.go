//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
	"strconv"
	"strings"
)

func main() {
	type CustomersItem struct {
		Id   int    `json:"id"`
		Name string `json:"name"`
	}

	var customers []CustomersItem = []CustomersItem{CustomersItem{
		Id:   1,
		Name: "Alice",
	}, CustomersItem{
		Id:   2,
		Name: "Bob",
	}}
	_ = customers
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
		CustomerId: 3,
		Total:      80,
	}}
	type Result struct {
		OrderId  any `json:"orderId"`
		Customer any `json:"customer"`
		Total    any `json:"total"`
	}

	type Result1 struct {
		OrderId  int           `json:"orderId"`
		Customer CustomersItem `json:"customer"`
		Total    int           `json:"total"`
	}

	var result []Result = _cast[[]Result](func() []Result1 {
		_res := []Result1{}
		for _, o := range orders {
			matched := false
			for _, c := range customers {
				if !(o.CustomerId == c.Id) {
					continue
				}
				matched = true
				_res = append(_res, Result1{
					OrderId:  o.Id,
					Customer: c,
					Total:    o.Total,
				})
			}
			if !matched {
				var c CustomersItem
				_res = append(_res, Result1{
					OrderId:  o.Id,
					Customer: c,
					Total:    o.Total,
				})
			}
		}
		return _res
	}())
	fmt.Println("--- Left Join ---")
	for _, entry := range result {
		fmt.Println(strings.TrimRight(strings.Join([]string{fmt.Sprint("Order"), fmt.Sprint(entry.OrderId), fmt.Sprint("customer"), fmt.Sprint(entry.Customer), fmt.Sprint("total"), fmt.Sprint(entry.Total)}, " "), " "))
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
