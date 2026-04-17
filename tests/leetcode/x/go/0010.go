package main

import "fmt"

func matchAt(s, p string, i, j int) bool {
    if j == len(p) {
        return i == len(s)
    }
    first := i < len(s) && (p[j] == '.' || s[i] == p[j])
    if j+1 < len(p) && p[j+1] == '*' {
        return matchAt(s, p, i, j+2) || (first && matchAt(s, p, i+1, j))
    }
    return first && matchAt(s, p, i+1, j+1)
}

func main() {
    var t int
    if _, err := fmt.Scan(&t); err != nil {
        return
    }
    for tc := 0; tc < t; tc++ {
        var s, p string
        fmt.Scan(&s)
        fmt.Scan(&p)
        if matchAt(s, p, 0, 0) {
            fmt.Print("true")
        } else {
            fmt.Print("false")
        }
        if tc+1 < t {
            fmt.Print("\n")
        }
    }
}
