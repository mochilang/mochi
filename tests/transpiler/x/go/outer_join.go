//go:build ignore

// Generated by Mochi v0.10.36 on 2025-07-22 18:26:38 GMT+7
package main

import (
	"fmt"
)

var customers []Customer = []Customer{Customer{
	ID:   1,
	Name: "Alice",
}, Customer{
	ID:   2,
	Name: "Bob",
}, Customer{
	ID:   3,
	Name: "Charlie",
}, Customer{
	ID:   4,
	Name: "Diana",
}}

type Customer struct {
	ID   int    `json:"id"`
	Name string `json:"name"`
}

var orders []Order = []Order{Order{
	ID:         100,
	CustomerID: 1,
	Total:      250,
}, Order{
	ID:         101,
	CustomerID: 2,
	Total:      125,
}, Order{
	ID:         102,
	CustomerID: 1,
	Total:      300,
}, Order{
	ID:         103,
	CustomerID: 5,
	Total:      80,
}}

type Order struct {
	ID         int `json:"id"`
	CustomerID int `json:"customerId"`
	Total      int `json:"total"`
}

type Result struct {
	Order    *Order    `json:"order"`
	Customer *Customer `json:"customer"`
}

func main() {
	var result []Result = func() []Result {
		res := []Result{}
		for _, oVal := range orders {
			o := &oVal
			matched := false
			for _, cVal := range customers {
				c := &cVal
				if o.CustomerID == c.ID {
					matched = true
					res = append(res, Result{
						Order:    o,
						Customer: c,
					})
				}
			}
			if !matched {
				var c *Customer = nil
				res = append(res, Result{
					Order:    o,
					Customer: c,
				})
			}
		}
		for _, cVal := range customers {
			c := &cVal
			exists := false
			for _, oVal2 := range orders {
				o := &oVal2
				if o.CustomerID == c.ID {
					exists = true
					break
				}
			}
			if !exists {
				var o *Order = nil
				res = append(res, Result{
					Order:    o,
					Customer: c,
				})
			}
		}
		return res
	}()
	fmt.Println("--- Outer Join using syntax ---")
	for _, row := range result {
		if row.Order != nil {
			if row.Customer != nil {
				fmt.Println("Order", row.(map[string]any)["order"].(map[string]any)["id"], "by", row.(map[string]any)["customer"].(map[string]any)["name"], "- $", row.(map[string]any)["order"].(map[string]any)["total"])
			} else {
				fmt.Println("Order", row.(map[string]any)["order"].(map[string]any)["id"], "by", "Unknown", "- $", row.(map[string]any)["order"].(map[string]any)["total"])
			}
		} else {
			fmt.Println("Customer", row.(map[string]any)["customer"].(map[string]any)["name"], "has no orders")
		}
	}
}
