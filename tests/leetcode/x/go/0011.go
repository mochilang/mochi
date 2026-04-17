package main

import "fmt"

func maxArea(h []int) int {
    left, right := 0, len(h)-1
    best := 0
    for left < right {
        height := h[left]
        if h[right] < height { height = h[right] }
        area := (right - left) * height
        if area > best { best = area }
        if h[left] < h[right] { left++ } else { right-- }
    }
    return best
}

func main() {
    var t int
    if _, err := fmt.Scan(&t); err != nil { return }
    for tc := 0; tc < t; tc++ {
        var n int
        fmt.Scan(&n)
        h := make([]int, n)
        for i := 0; i < n; i++ { fmt.Scan(&h[i]) }
        fmt.Print(maxArea(h))
        if tc+1 < t { fmt.Print("\n") }
    }
}
