package main

import (
    "bufio"
    "fmt"
    "os"
    "strings"
)

func expand(s string, left, right int) (int, int) {
    for left >= 0 && right < len(s) && s[left] == s[right] {
        left--
        right++
    }
    return left + 1, right - left - 1
}

func longestPalindrome(s string) string {
    bestStart, bestLen := 0, 0
    if len(s) > 0 {
        bestLen = 1
    }
    for i := 0; i < len(s); i++ {
        start, length := expand(s, i, i)
        if length > bestLen {
            bestStart, bestLen = start, length
        }
        start, length = expand(s, i, i+1)
        if length > bestLen {
            bestStart, bestLen = start, length
        }
    }
    return s[bestStart : bestStart+bestLen]
}

func main() {
    reader := bufio.NewReader(os.Stdin)
    line, err := reader.ReadString('\n')
    if err != nil && len(line) == 0 {
        return
    }
    var t int
    fmt.Sscanf(strings.TrimSpace(line), "%d", &t)
    out := make([]string, 0, t)
    for i := 0; i < t; i++ {
        s, _ := reader.ReadString('\n')
        s = strings.TrimRight(s, "\r\n")
        out = append(out, longestPalindrome(s))
    }
    fmt.Print(strings.Join(out, "\n"))
}
