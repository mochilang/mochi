package main

import "fmt"

const (
    intMin = -2147483648
    intMax = 2147483647
)

func reverseInt(x int) int {
    ans := 0
    for x != 0 {
        digit := x % 10
        x /= 10
        if ans > intMax/10 || (ans == intMax/10 && digit > 7) {
            return 0
        }
        if ans < intMin/10 || (ans == intMin/10 && digit < -8) {
            return 0
        }
        ans = ans*10 + digit
    }
    return ans
}

func main() {
    var t int
    if _, err := fmt.Scan(&t); err != nil { return }
    for i := 0; i < t; i++ {
        var x int
        fmt.Scan(&x)
        fmt.Print(reverseInt(x))
        if i+1 < t { fmt.Print("\n") }
    }
}
