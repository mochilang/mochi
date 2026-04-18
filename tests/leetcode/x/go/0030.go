package main
import (
  "fmt"
  "sort"
  "strings"
)
func solveCase(s string, words []string) []int {
  if len(words) == 0 { return []int{} }
  wlen := len(words[0])
  total := wlen * len(words)
  target := append([]string(nil), words...)
  sort.Strings(target)
  ans := []int{}
  for i := 0; i+total <= len(s); i++ {
    parts := make([]string, len(words))
    for j := 0; j < len(words); j++ { parts[j] = s[i+j*wlen : i+(j+1)*wlen] }
    sort.Strings(parts)
    ok := true
    for j := range parts { if parts[j] != target[j] { ok = false; break } }
    if ok { ans = append(ans, i) }
  }
  return ans
}
func fmtList(arr []int) string { parts:=make([]string,len(arr)); for i,v:= range arr { parts[i]=fmt.Sprint(v)}; return "["+strings.Join(parts,",")+"]" }
func main(){ var t int; if _,err:=fmt.Scan(&t); err!=nil { return }; out:=[]string{}; for ; t>0; t-- { var s string; var m int; fmt.Scan(&s); fmt.Scan(&m); words:=make([]string,m); for i:=0;i<m;i++{fmt.Scan(&words[i])}; out=append(out, fmtList(solveCase(s,words))) }; fmt.Print(strings.Join(out,"\n")) }
