package main

import (
  "bufio"
  "fmt"
  "os"
  "strconv"
  "strings"
)

func isMatch(s, p string) bool {
  i, j, star, match := 0, 0, -1, 0
  for i < len(s) {
    if j < len(p) && (p[j] == '?' || p[j] == s[i]) { i++; j++ } else if j < len(p) && p[j] == '*' { star = j; match = i; j++ } else if star != -1 { j = star + 1; match++; i = match } else { return false }
  }
  for j < len(p) && p[j] == '*' { j++ }
  return j == len(p)
}

func main() {
  scanner := bufio.NewScanner(os.Stdin)
  lines := []string{}
  for scanner.Scan() { lines = append(lines, scanner.Text()) }
  if len(lines) == 0 || strings.TrimSpace(lines[0]) == "" { return }
  idx := 0
  t, _ := strconv.Atoi(strings.TrimSpace(lines[idx])); idx++
  out := make([]string, 0, t)
  for tc := 0; tc < t; tc++ {
    n, _ := strconv.Atoi(strings.TrimSpace(lines[idx])); idx++
    s := ""; if n > 0 { s = lines[idx]; idx++ }
    m, _ := strconv.Atoi(strings.TrimSpace(lines[idx])); idx++
    p := ""; if m > 0 { p = lines[idx]; idx++ }
    if isMatch(s, p) { out = append(out, "true") } else { out = append(out, "false") }
  }
  fmt.Print(strings.Join(out, "\n"))
}
