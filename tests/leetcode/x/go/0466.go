package main
import("bufio";"fmt";"os")
type pair struct{s1,s2 int}
func solve(s1 string,n1 int,s2 string,n2 int)int{set:=map[byte]bool{};for i:=0;i<len(s1);i++{set[s1[i]]=true};for i:=0;i<len(s2);i++{if !set[s2[i]]{return 0}};seen:=map[int]pair{};s1c,s2c,idx2:=0,0,0;prefixS1,prefixS2,loopS1,loopS2:=0,0,0,0;for{ s1c++; for i:=0;i<len(s1);i++{if s1[i]==s2[idx2]{idx2++; if idx2==len(s2){s2c++; idx2=0}}}; if s1c==n1{return s2c/n2}; if p,ok:=seen[idx2];ok{prefixS1,prefixS2=p.s1,p.s2;loopS1=s1c-p.s1;loopS2=s2c-p.s2;break}; seen[idx2]=pair{s1c,s2c}}
ans:=prefixS2; remain:=n1-prefixS1; ans+=(remain/loopS1)*loopS2; rest:=remain%loopS1; for ;rest>0;rest--{for i:=0;i<len(s1);i++{if s1[i]==s2[idx2]{idx2++; if idx2==len(s2){ans++; idx2=0}}}}; return ans/n2}
func main(){in:=bufio.NewReader(os.Stdin);out:=bufio.NewWriter(os.Stdout);defer out.Flush();var t int;if _,e:=fmt.Fscan(in,&t);e!=nil{return};for tc:=0;tc<t;tc++{var s1,s2 string;var n1,n2 int;fmt.Fscan(in,&s1,&n1,&s2,&n2);if tc>0{fmt.Fprintln(out);fmt.Fprintln(out)};fmt.Fprint(out,solve(s1,n1,s2,n2))}}
