//go:build ignore

package main

import (
	"fmt"
	"strings"
)

func main() {
	var customers []map[string]any = []map[string]any{map[string]any{"id": 1, "name": "Alice"}, map[string]any{"id": 2, "name": "Bob"}, map[string]any{"id": 3, "name": "Charlie"}}
	_ = customers
	var orders []map[string]int = []map[string]int{map[string]int{
		"id":         100,
		"customerId": 1,
		"total":      250,
	}, map[string]int{
		"id":         101,
		"customerId": 2,
		"total":      125,
	}, map[string]int{
		"id":         102,
		"customerId": 1,
		"total":      300,
	}}
	var result []map[string]int = func() []map[string]any {
		_res := []map[string]any{}
		for _, o := range orders {
			for _, c := range customers {
				_res = append(_res, map[string]any{
					"orderId":            o["id"],
					"orderCustomerId":    o["customerId"],
					"pairedCustomerName": c["name"],
					"orderTotal":         o["total"],
				})
			}
		}
		return _res
	}()
	fmt.Println("--- Cross Join: All order-customer pairs ---")
	for _, entry := range result {
		fmt.Println(strings.TrimRight(strings.Join([]string{fmt.Sprint("Order"), fmt.Sprint(entry["orderId"]), fmt.Sprint("(customerId:"), fmt.Sprint(entry["orderCustomerId"]), fmt.Sprint(", total: $"), fmt.Sprint(entry["orderTotal"]), fmt.Sprint(") paired with"), fmt.Sprint(entry["pairedCustomerName"])}, " "), " "))
	}
}
