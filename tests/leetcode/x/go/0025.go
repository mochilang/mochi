package main

import (
    "fmt"
    "strings"
)

func revGroups(arr []int, k int) []int {
    out := append([]int(nil), arr...)
    for i := 0; i+k <= len(out); i += k {
        for l, r := i, i+k-1; l < r; l, r = l+1, r-1 {
            out[l], out[r] = out[r], out[l]
        }
    }
    return out
}

func fmtList(arr []int) string {
    parts := make([]string, len(arr))
    for i, v := range arr { parts[i] = fmt.Sprint(v) }
    return "[" + strings.Join(parts, ",") + "]"
}

func main() {
    var t int
    if _, err := fmt.Scan(&t); err != nil { return }
    out := make([]string, 0, t)
    for ; t > 0; t-- {
        var n, k int
        fmt.Scan(&n)
        arr := make([]int, n)
        for i := 0; i < n; i++ { fmt.Scan(&arr[i]) }
        fmt.Scan(&k)
        out = append(out, fmtList(revGroups(arr, k)))
    }
    fmt.Print(strings.Join(out, "\n"))
}
