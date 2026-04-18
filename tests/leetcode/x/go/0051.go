package main

import (
  "bufio"
  "fmt"
  "os"
  "strconv"
  "strings"
)

func solve(n int) [][]string {
  cols := make([]bool, n)
  d1 := make([]bool, 2*n)
  d2 := make([]bool, 2*n)
  board := make([][]byte, n)
  for i := 0; i < n; i++ { board[i] = []byte(strings.Repeat(".", n)) }
  res := [][]string{}
  var dfs func(int)
  dfs = func(r int) {
    if r == n { sol := make([]string, n); for i := 0; i < n; i++ { sol[i] = string(board[i]) }; res = append(res, sol); return }
    for c := 0; c < n; c++ { a, b := r+c, r-c+n-1; if cols[c] || d1[a] || d2[b] { continue }; cols[c], d1[a], d2[b] = true, true, true; board[r][c] = 'Q'; dfs(r+1); board[r][c] = '.'; cols[c], d1[a], d2[b] = false, false, false }
  }
  dfs(0); return res
}

func main() {
  scanner := bufio.NewScanner(os.Stdin); lines := []string{}; for scanner.Scan() { lines = append(lines, strings.TrimSpace(scanner.Text())) }
  if len(lines) == 0 || lines[0] == "" { return }
  idx := 0; t, _ := strconv.Atoi(lines[idx]); idx++
  out := []string{}
  for tc := 0; tc < t; tc++ { n, _ := strconv.Atoi(lines[idx]); idx++; sols := solve(n); out = append(out, strconv.Itoa(len(sols))); for si, sol := range sols { out = append(out, sol...); if si+1 < len(sols) { out = append(out, "-") } }; if tc+1 < t { out = append(out, "=") } }
  fmt.Print(strings.Join(out, "\n"))
}
