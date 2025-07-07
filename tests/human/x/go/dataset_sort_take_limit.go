package main

import (
    "fmt"
    "sort"
)

type Product struct {
    Name  string
    Price int
}

func main() {
    products := []Product{
        {"Laptop", 1500},
        {"Smartphone", 900},
        {"Tablet", 600},
        {"Monitor", 300},
        {"Keyboard", 100},
        {"Mouse", 50},
        {"Headphones", 200},
    }

    sort.Slice(products, func(i, j int) bool {
        return products[i].Price > products[j].Price
    })

    expensive := products[1:4]

    fmt.Println("--- Top products (excluding most expensive) ---")
    for _, item := range expensive {
        fmt.Printf("%s costs $%d\n", item.Name, item.Price)
    }
}
