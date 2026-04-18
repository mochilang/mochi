package main

import "fmt"

func maxProfit(prices []int) int {
    if len(prices) == 0 {
        return 0
    }
    minPrice, best := prices[0], 0
    for _, p := range prices[1:] {
        if p-minPrice > best {
            best = p - minPrice
        }
        if p < minPrice {
            minPrice = p
        }
    }
    return best
}

func main() {
    var t int
    if _, err := fmt.Scan(&t); err != nil {
        return
    }
    for tc := 0; tc < t; tc++ {
        var n int
        fmt.Scan(&n)
        prices := make([]int, n)
        for i := 0; i < n; i++ {
            fmt.Scan(&prices[i])
        }
        fmt.Print(maxProfit(prices))
        if tc+1 < t {
            fmt.Print("\n")
        }
    }
}
