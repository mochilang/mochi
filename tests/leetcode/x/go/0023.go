package main

import (
    "fmt"
    "sort"
    "strings"
)

func main() {
    var t int
    if _, err := fmt.Scan(&t); err != nil { return }
    out := make([]string, 0, t)
    for ; t > 0; t-- {
        var k int
        fmt.Scan(&k)
        vals := []int{}
        for i := 0; i < k; i++ {
            var n int
            fmt.Scan(&n)
            for j := 0; j < n; j++ {
                var x int
                fmt.Scan(&x)
                vals = append(vals, x)
            }
        }
        sort.Ints(vals)
        parts := make([]string, len(vals))
        for i, v := range vals { parts[i] = fmt.Sprint(v) }
        out = append(out, "["+strings.Join(parts, ",")+"]")
    }
    fmt.Print(strings.Join(out, "\n"))
}
