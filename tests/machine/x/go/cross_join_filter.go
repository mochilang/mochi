//go:build ignore

package main

import "fmt"

type Pair struct {
    N int
    L string
}

func main() {
    nums := []int{1, 2, 3}
    letters := []string{"A", "B"}
    pairs := func() []Pair {
        results := []Pair{}
        for _, n := range nums {
            if n%2 == 0 {
                for _, l := range letters {
                    results = append(results, Pair{n, l})
                }
            }
        }
        return results
    }()
    fmt.Println("--- Even pairs ---")
    for _, p := range pairs {
        fmt.Println(p.N, p.L)
    }
}
