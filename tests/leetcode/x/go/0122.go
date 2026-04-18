package main

import "fmt"

func maxProfit(prices []int) int {
    best := 0
    for i := 1; i < len(prices); i++ {
        if prices[i] > prices[i-1] {
            best += prices[i] - prices[i-1]
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
