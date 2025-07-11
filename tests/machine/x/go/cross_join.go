//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
	"reflect"
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
	}, CustomersItem{
		Id:   3,
		Name: "Charlie",
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
		CustomerId: 2,
		Total:      125,
	}, OrdersItem{
		Id:         102,
		CustomerId: 1,
		Total:      300,
	}}
	type Result struct {
		OrderId            any `json:"orderId"`
		OrderCustomerId    any `json:"orderCustomerId"`
		PairedCustomerName any `json:"pairedCustomerName"`
		OrderTotal         any `json:"orderTotal"`
	}

	var result []Result = func() []Result {
		_res := []Result{}
		for _, o := range orders {
			for _, c := range customers {
				_res = append(_res, Result{
					OrderId:            o.Id,
					OrderCustomerId:    o.CustomerId,
					PairedCustomerName: c.Name,
					OrderTotal:         o.Total,
				})
			}
		}
		return _res
	}()
	fmt.Println(_fmt("--- Cross Join: All order-customer pairs ---"))
	for _, entry := range result {
		fmt.Println(strings.TrimRight(strings.Join([]string{_fmt("Order"), _fmt(entry.OrderId), _fmt("(customerId:"), _fmt(entry.OrderCustomerId), _fmt(", total: $"), _fmt(entry.OrderTotal), _fmt(") paired with"), _fmt(entry.PairedCustomerName)}, " "), " "))
	}
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
