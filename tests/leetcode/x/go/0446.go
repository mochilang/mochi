package main
import("bufio";"fmt";"os")
func count(a []int64)int64{dp:=make([]map[int64]int64,len(a));for i:=range dp{dp[i]=map[int64]int64{}};ans:=int64(0);for i,x:=range a{for j:=0;j<i;j++{d:=x-a[j];prev:=dp[j][d];ans+=prev;dp[i][d]+=prev+1}};return ans}
func main(){in:=bufio.NewReader(os.Stdin);out:=bufio.NewWriter(os.Stdout);defer out.Flush();var t int;if _,e:=fmt.Fscan(in,&t);e!=nil{return};for tc:=0;tc<t;tc++{var n int;fmt.Fscan(in,&n);a:=make([]int64,n);for i:=range a{fmt.Fscan(in,&a[i])};if tc>0{fmt.Fprintln(out);fmt.Fprintln(out)};fmt.Fprint(out,count(a))}}
