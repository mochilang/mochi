package main
import "fmt"
func solveCase(s string) int { stack:=[]int{-1}; best:=0; for i,ch:= range s { if ch=='(' { stack=append(stack,i) } else { stack=stack[:len(stack)-1]; if len(stack)==0 { stack=append(stack,i) } else if i-stack[len(stack)-1] > best { best=i-stack[len(stack)-1] } } }; return best }
func main(){ var t int; if _,err:=fmt.Scan(&t); err!=nil { return }; out:=make([]int,0,t); for ; t>0; t-- { var n int; fmt.Scan(&n); s:=""; if n>0 { fmt.Scan(&s) }; out=append(out, solveCase(s)) }; for i,v := range out { if i>0 { fmt.Print("\n") }; fmt.Print(v) } }
