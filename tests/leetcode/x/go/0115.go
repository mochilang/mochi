package main

import "fmt"

func solve(s, t string) int {
    dp := make([]int, len(t)+1)
    dp[0] = 1
    for i := 0; i < len(s); i++ {
        for j := len(t); j >= 1; j-- {
            if s[i] == t[j-1] {
                dp[j] += dp[j-1]
            }
        }
    }
    return dp[len(t)]
}

func main() {
    var tc int
    if _, err := fmt.Scan(&tc); err != nil { return }
    for i := 0; i < tc; i++ {
        var s, t string
        fmt.Scan(&s, &t)
        fmt.Print(solve(s, t))
        if i+1 < tc { fmt.Print("\n") }
    }
}
