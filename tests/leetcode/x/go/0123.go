package main

import "fmt"

func maxProfit(prices []int) int {
    buy1, sell1, buy2, sell2 := -1000000000, 0, -1000000000, 0
    for _, p := range prices {
        if -p > buy1 {
            buy1 = -p
        }
        if buy1+p > sell1 {
            sell1 = buy1 + p
        }
        if sell1-p > buy2 {
            buy2 = sell1 - p
        }
        if buy2+p > sell2 {
            sell2 = buy2 + p
        }
    }
    return sell2
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
