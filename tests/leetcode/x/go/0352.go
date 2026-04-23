package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strings"
)
func snapshot(seen map[int]bool) [][2]int { vals:=make([]int,0,len(seen)); for v:=range seen{vals=append(vals,v)}; sort.Ints(vals); out:=[][2]int{}; for _,v:=range vals{ if len(out)==0 || v>out[len(out)-1][1]+1{out=append(out,[2]int{v,v})}else{out[len(out)-1][1]=v} }; return out }
func fmtRanges(r [][2]int) string { parts:=make([]string,len(r)); for i,p:=range r{parts[i]=fmt.Sprintf("[%d,%d]",p[0],p[1])}; return "["+strings.Join(parts,",")+"]" }
func main(){in:=bufio.NewReader(os.Stdin); out:=bufio.NewWriter(os.Stdout); defer out.Flush(); var t int; if _,err:=fmt.Fscan(in,&t);err!=nil{return}; cases:=make([]string,t); for tc:=0;tc<t;tc++{var ops int; fmt.Fscan(in,&ops); seen:=map[int]bool{}; snaps:=[]string{}; for i:=0;i<ops;i++{var op string; fmt.Fscan(in,&op); if op=="A"{var v int; fmt.Fscan(in,&v); seen[v]=true}else{snaps=append(snaps,fmtRanges(snapshot(seen)))}}; cases[tc]="["+strings.Join(snaps,",")+"]"}; fmt.Fprint(out,strings.Join(cases,"\n\n"))}
