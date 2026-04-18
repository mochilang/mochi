package main

import (
  "bufio"
  "fmt"
  "os"
  "strconv"
  "strings"
)

func getPermutation(n int, k int) string {
  digits := make([]string, n)
  for i := 0; i < n; i++ { digits[i] = strconv.Itoa(i + 1) }
  fact := make([]int, n + 1)
  fact[0] = 1
  for i := 1; i <= n; i++ { fact[i] = fact[i - 1] * i }
  k--
  var out strings.Builder
  for rem := n; rem >= 1; rem-- {
    block := fact[rem - 1]
    idx := k / block
    k %= block
    out.WriteString(digits[idx])
    digits = append(digits[:idx], digits[idx+1:]...)
  }
  return out.String()
}

func main() {
  scanner := bufio.NewScanner(os.Stdin)
  lines := []string{}
  for scanner.Scan() { lines = append(lines, strings.TrimSpace(scanner.Text())) }
  if len(lines) == 0 || lines[0] == "" { return }
  idx := 0
  t, _ := strconv.Atoi(lines[idx]); idx++
  out := []string{}
  for tc := 0; tc < t; tc++ {
    n, _ := strconv.Atoi(lines[idx]); idx++
    k, _ := strconv.Atoi(lines[idx]); idx++
    out = append(out, getPermutation(n, k))
  }
  fmt.Print(strings.Join(out, "\n"))
}
