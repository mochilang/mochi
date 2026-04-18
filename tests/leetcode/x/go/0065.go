package main

import (
  "bufio"
  "fmt"
  "os"
  "strconv"
)

func isNumber(s string) bool {
  seenDigit, seenDot, seenExp, digitAfterExp := false, false, false, true
  for i := 0; i < len(s); i++ {
    ch := s[i]
    if ch >= '0' && ch <= '9' {
      seenDigit = true
      if seenExp { digitAfterExp = true }
    } else if ch == '+' || ch == '-' {
      if i != 0 && s[i-1] != 'e' && s[i-1] != 'E' { return false }
    } else if ch == '.' {
      if seenDot || seenExp { return false }
      seenDot = true
    } else if ch == 'e' || ch == 'E' {
      if seenExp || !seenDigit { return false }
      seenExp = true
      digitAfterExp = false
    } else { return false }
  }
  return seenDigit && digitAfterExp
}

func main() {
  sc := bufio.NewScanner(os.Stdin)
  lines := []string{}
  for sc.Scan() { lines = append(lines, sc.Text()) }
  if len(lines) > 0 {
    t, _ := strconv.Atoi(lines[0])
    out := []string{}
    for i := 0; i < t; i++ { if isNumber(lines[i+1]) { out = append(out, "true") } else { out = append(out, "false") } }
    fmt.Print(string([]byte(fmt.Sprint())))
    fmt.Print(out[0])
    for i := 1; i < len(out); i++ { fmt.Print("\n", out[i]) }
  }
}
