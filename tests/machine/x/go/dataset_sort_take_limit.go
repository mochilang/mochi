//go:build ignore

package main

import (
    "fmt"
    "sort"
)

type ProductsItem struct {
    Name  string `json:"name"`
    Price int    `json:"price"`
}

func main() {
    products := []ProductsItem{{"Laptop", 1500}, {"Smartphone", 900}, {"Tablet", 600}, {"Monitor", 300}, {"Keyboard", 100}, {"Mouse", 50}, {"Headphones", 200}}
    expensive := func() []ProductsItem {
        src := _toAnySlice(products)
        resAny := _query(src, []_joinSpec{}, _queryOpts{selectFn: func(_a ...any) any { p := _cast[ProductsItem](_a[0]); _ = p; return p }, sortKey: func(_a ...any) any { p := _cast[ProductsItem](_a[0]); _ = p; return -p.Price }, skip: 1, take: 3})
        out := make([]ProductsItem, len(resAny))
        for i, v := range resAny {
            out[i] = _cast[ProductsItem](v)
        }
        return out
    }()
    fmt.Println("--- Top products (excluding most expensive) ---")
    for _, item := range expensive {
        fmt.Println(item.Name, "costs $", item.Price)
    }
}
