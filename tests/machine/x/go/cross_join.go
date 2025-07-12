//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

func main() {
	var customers []CustomersItem = []CustomersItem{CustomersItem{
		1,
		"Alice",
	}, CustomersItem{
		2,
		"Bob",
	}, CustomersItem{
		3,
		"Charlie",
	}}
	_ = customers
	var orders []OrdersItem = []OrdersItem{OrdersItem{
		100,
		1,
		250,
	}, OrdersItem{
		101,
		2,
		125,
	}, OrdersItem{
		102,
		1,
		300,
	}}
	var result []Result = func() []Result {
		results := []Result{}
		for _, o := range orders {
			for _, c := range customers {
				results = append(results, Result{
					o.Id,
					o.CustomerId,
					c.Name,
					o.Total,
				})
			}
		}
		return results
	}()
	fmt.Println("--- Cross Join: All order-customer pairs ---")
	for _, entry := range result {
		_print("Order", entry.OrderId, "(customerId:", entry.OrderCustomerId, ", total: $", entry.OrderTotal, ") paired with", entry.PairedCustomerName)
	}
}

func _print(args ...any) {
	first := true
	for _, a := range args {
		if !first {
			fmt.Print(" ")
		}
		first = false
		rv := reflect.ValueOf(a)
		if a == nil || ((rv.Kind() == reflect.Map || rv.Kind() == reflect.Slice) && rv.IsNil()) {
			fmt.Print("<nil>")
			continue
		}
		if rv.Kind() == reflect.Slice && rv.Type().Elem().Kind() != reflect.Uint8 {
			for i := 0; i < rv.Len(); i++ {
				if i > 0 {
					fmt.Print(" ")
				}
				fmt.Print(_sprint(rv.Index(i).Interface()))
			}
			continue
		}
		fmt.Print(_sprint(a))
	}
	fmt.Println()
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
