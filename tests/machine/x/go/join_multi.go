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
	}}
	_ = customers
	var orders []OrdersItem = []OrdersItem{OrdersItem{
		100,
		1,
	}, OrdersItem{
		101,
		2,
	}}
	var items []ItemsItem = []ItemsItem{ItemsItem{
		100,
		"a",
	}, ItemsItem{
		101,
		"b",
	}}
	_ = items
	var result []Result = func() []Result {
		results := []Result{}
		for _, o := range orders {
			for _, c := range customers {
				if !(o.CustomerId == c.Id) {
					continue
				}
				for _, i := range items {
					if !(o.Id == i.OrderId) {
						continue
					}
					results = append(results, Result{
						c.Name,
						i.Sku,
					})
				}
			}
		}
		return results
	}()
	fmt.Println("--- Multi Join ---")
	for _, r := range result {
		_print(r.Name, "bought item", r.Sku)
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
