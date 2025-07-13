//go:build ignore

package main

import "fmt"

type CustomersItem struct {
    Id   int    `json:"id"`
    Name string `json:"name"`
}

type OrdersItem struct {
    Id         int `json:"id"`
    CustomerId int `json:"customerId"`
    Total      int `json:"total"`
}

type Result struct {
    OrderId         int    `json:"orderId"`
    OrderCustomerId int    `json:"orderCustomerId"`
    PairedCustomerName string `json:"pairedCustomerName"`
    OrderTotal      int    `json:"orderTotal"`
}

func main() {
    customers := []CustomersItem{{1, "Alice"}, {2, "Bob"}, {3, "Charlie"}}
    orders := []OrdersItem{{100, 1, 250}, {101, 2, 125}, {102, 1, 300}}
    result := func() []Result {
        results := []Result{}
        for _, o := range orders {
            for _, c := range customers {
                results = append(results, Result{o.Id, o.CustomerId, c.Name, o.Total})
            }
        }
        return results
    }()
    fmt.Println("--- Cross Join: All order-customer pairs ---")
    for _, entry := range result {
        fmt.Println("Order", entry.OrderId, "(customerId:", entry.OrderCustomerId, ", total: $", entry.OrderTotal, ") paired with", entry.PairedCustomerName)
    }
}
