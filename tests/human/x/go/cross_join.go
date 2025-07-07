package main

import "fmt"

type Customer struct {
    ID   int
    Name string
}

type Order struct {
    ID         int
    CustomerID int
    Total      int
}

func main() {
    customers := []Customer{{1, "Alice"}, {2, "Bob"}, {3, "Charlie"}}
    orders := []Order{{100, 1, 250}, {101, 2, 125}, {102, 1, 300}}

    var result []struct {
        OrderID         int
        OrderCustomerID int
        PairedCustomer  string
        OrderTotal      int
    }

    for _, o := range orders {
        for _, c := range customers {
            result = append(result, struct {
                OrderID         int
                OrderCustomerID int
                PairedCustomer  string
                OrderTotal      int
            }{o.ID, o.CustomerID, c.Name, o.Total})
        }
    }

    fmt.Println("--- Cross Join: All order-customer pairs ---")
    for _, entry := range result {
        fmt.Printf("Order %d (customerId: %d, total: $%d) paired with %s\n",
            entry.OrderID, entry.OrderCustomerID, entry.OrderTotal, entry.PairedCustomer)
    }
}
