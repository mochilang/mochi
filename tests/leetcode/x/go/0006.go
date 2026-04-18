package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
    "strings"
)

func convert(s string, numRows int) string {
    if numRows <= 1 || numRows >= len(s) {
        return s
    }
    cycle := 2*numRows - 2
    var b strings.Builder
    b.Grow(len(s))
    for row := 0; row < numRows; row++ {
        for i := row; i < len(s); i += cycle {
            b.WriteByte(s[i])
            diag := i + cycle - 2*row
            if row > 0 && row < numRows-1 && diag < len(s) {
                b.WriteByte(s[diag])
            }
        }
    }
    return b.String()
}

func main() {
    reader := bufio.NewReader(os.Stdin)
    first, err := reader.ReadString('\n')
    if err != nil && len(first) == 0 {
        return
    }
    t, _ := strconv.Atoi(strings.TrimSpace(first))
    out := make([]string, 0, t)
    for i := 0; i < t; i++ {
        s, _ := reader.ReadString('\n')
        s = strings.TrimRight(s, "\r\n")
        line, _ := reader.ReadString('\n')
        numRows, _ := strconv.Atoi(strings.TrimSpace(line))
        out = append(out, convert(s, numRows))
    }
    fmt.Print(strings.Join(out, "\n"))
}
