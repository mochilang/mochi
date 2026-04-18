package main

import "fmt"

func solve(vals []int, ok []bool) int {
    best := -1000000000
    var dfs func(int) int
    dfs = func(i int) int {
        if i >= len(vals) || !ok[i] {
            return 0
        }
        left := dfs(2*i + 1)
        if left < 0 { left = 0 }
        right := dfs(2*i + 2)
        if right < 0 { right = 0 }
        total := vals[i] + left + right
        if total > best { best = total }
        if left > right { return vals[i] + left }
        return vals[i] + right
    }
    dfs(0)
    return best
}

func main() {
    var tc int
    if _, err := fmt.Scan(&tc); err != nil { return }
    for t := 0; t < tc; t++ {
        var n int
        fmt.Scan(&n)
        vals := make([]int, n)
        ok := make([]bool, n)
        for i := 0; i < n; i++ {
            var tok string
            fmt.Scan(&tok)
            if tok != "null" {
                ok[i] = true
                fmt.Sscan(tok, &vals[i])
            }
        }
        fmt.Print(solve(vals, ok))
        if t+1 < tc { fmt.Print("\n") }
    }
}
