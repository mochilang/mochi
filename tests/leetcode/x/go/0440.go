package main
import("bufio";"fmt";"os")
func count(n,p int64)int64{steps:=int64(0);first,next:=p,p+1;for first<=n{limit:=next;if n+1<limit{limit=n+1};steps+=limit-first;first*=10;next*=10};return steps}
func kth(n,k int64)int64{cur:=int64(1);k--;for k>0{steps:=count(n,cur);if steps<=k{cur++;k-=steps}else{cur*=10;k--}};return cur}
func main(){in:=bufio.NewReader(os.Stdin);out:=bufio.NewWriter(os.Stdout);defer out.Flush();var t int;if _,e:=fmt.Fscan(in,&t);e!=nil{return};for tc:=0;tc<t;tc++{var n,k int64;fmt.Fscan(in,&n,&k);if tc>0{fmt.Fprintln(out);fmt.Fprintln(out)};fmt.Fprint(out,kth(n,k))}}
