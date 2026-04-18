package main

import (
    "bufio"
    "fmt"
    "os"
    "strings"
)

func myAtoi(s string) int {
    i := 0
    for i < len(s) && s[i] == ' ' { i++ }
    sign := 1
    if i < len(s) && (s[i] == '+' || s[i] == '-') {
        if s[i] == '-' { sign = -1 }
        i++
    }
    ans := 0
    limit := 7
    if sign < 0 { limit = 8 }
    for i < len(s) && s[i] >= '0' && s[i] <= '9' {
        digit := int(s[i] - '0')
        if ans > 214748364 || (ans == 214748364 && digit > limit) {
            if sign > 0 { return 2147483647 }
            return -2147483648
        }
        ans = ans*10 + digit
        i++
    }
    return sign * ans
}

func main() {
    reader := bufio.NewReader(os.Stdin)
    var t int
    fmt.Fscan(reader, &t)
    reader.ReadString('\n')
    out := make([]string, 0, t)
    for i := 0; i < t; i++ {
        s, _ := reader.ReadString('\n')
        s = strings.TrimRight(s, "\r\n")
        out = append(out, fmt.Sprint(myAtoi(s)))
    }
    fmt.Print(strings.Join(out, "\n"))
}
