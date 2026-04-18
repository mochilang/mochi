package main

import "fmt"

func median(a, b []int) float64 {
    m := make([]int, 0, len(a)+len(b))
    i, j := 0, 0
    for i < len(a) && j < len(b) {
        if a[i] <= b[j] { m = append(m, a[i]); i++ } else { m = append(m, b[j]); j++ }
    }
    m = append(m, a[i:]...)
    m = append(m, b[j:]...)
    n := len(m)
    if n%2 == 1 { return float64(m[n/2]) }
    return float64(m[n/2-1]+m[n/2]) / 2.0
}

func main() {
    var t int
    if _, err := fmt.Scan(&t); err != nil { return }
    for tc := 0; tc < t; tc++ {
        var n, m int
        fmt.Scan(&n)
        a := make([]int, n)
        for i := 0; i < n; i++ { fmt.Scan(&a[i]) }
        fmt.Scan(&m)
        b := make([]int, m)
        for i := 0; i < m; i++ { fmt.Scan(&b[i]) }
        fmt.Printf("%.1f", median(a,b))
        if tc + 1 < t { fmt.Print("\n") }
    }
}
