package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
    "strings"
)

func firstMissingPositive(nums []int) int {
    n := len(nums)
    i := 0
    for i < n {
        v := nums[i]
        if v >= 1 && v <= n && nums[v-1] != v {
            nums[i], nums[v-1] = nums[v-1], nums[i]
        } else {
            i++
        }
    }
    for i, v := range nums {
        if v != i+1 {
            return i + 1
        }
    }
    return n + 1
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    lines := []string{}
    for scanner.Scan() {
        lines = append(lines, strings.TrimSpace(scanner.Text()))
    }
    if len(lines) == 0 || lines[0] == "" {
        return
    }
    idx := 0
    t, _ := strconv.Atoi(lines[idx])
    idx++
    out := make([]string, 0, t)
    for tc := 0; tc < t; tc++ {
        n, _ := strconv.Atoi(lines[idx])
        idx++
        nums := make([]int, n)
        for i := 0; i < n; i++ {
            nums[i], _ = strconv.Atoi(lines[idx])
            idx++
        }
        out = append(out, strconv.Itoa(firstMissingPositive(nums)))
    }
    fmt.Print(strings.Join(out, "\n"))
}
