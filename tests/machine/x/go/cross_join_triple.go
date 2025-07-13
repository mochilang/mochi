//go:build ignore

package main

import "fmt"

type Combo struct {
    N int
    L string
    B bool
}

func main() {
    nums := []int{1, 2}
    letters := []string{"A", "B"}
    bools := []bool{true, false}
    combos := func() []Combo {
        results := []Combo{}
        for _, n := range nums {
            for _, l := range letters {
                for _, b := range bools {
                    results = append(results, Combo{n, l, b})
                }
            }
        }
        return results
    }()
    fmt.Println("--- Cross Join of three lists ---")
    for _, c := range combos {
        fmt.Println(c.N, c.L, c.B)
    }
}
