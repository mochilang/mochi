package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
)
type env struct{ w,h int }
func maxEnvelopes(a []env) int{sort.Slice(a,func(i,j int)bool{if a[i].w==a[j].w{return a[i].h>a[j].h};return a[i].w<a[j].w});tails:=[]int{};for _,e:=range a{i:=sort.SearchInts(tails,e.h);if i==len(tails){tails=append(tails,e.h)}else{tails[i]=e.h}};return len(tails)}
func main(){in:=bufio.NewReader(os.Stdin);out:=bufio.NewWriter(os.Stdout);defer out.Flush();var t int;if _,err:=fmt.Fscan(in,&t);err!=nil{return};for tc:=0;tc<t;tc++{var n int;fmt.Fscan(in,&n);a:=make([]env,n);for i:=range a{fmt.Fscan(in,&a[i].w,&a[i].h)};if tc>0{fmt.Fprintln(out);fmt.Fprintln(out)};fmt.Fprint(out,maxEnvelopes(a))}}
