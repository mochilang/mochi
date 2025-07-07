package main

import "fmt"

func main() {
    nums := []float64{1, 2, 3}
    var sum float64
    for _, n := range nums {
        sum += n
    }
    fmt.Println(sum / float64(len(nums)))
}
