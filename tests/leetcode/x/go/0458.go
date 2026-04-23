package main
import("bufio";"fmt";"os")
func pigs(b,d,t int)int{states:=t/d+1;p,cap:=0,1;for cap<b{p++;cap*=states};return p}
func main(){in:=bufio.NewReader(os.Stdin);out:=bufio.NewWriter(os.Stdout);defer out.Flush();var tcases int;if _,e:=fmt.Fscan(in,&tcases);e!=nil{return};for tc:=0;tc<tcases;tc++{var b,d,t int;fmt.Fscan(in,&b,&d,&t);if tc>0{fmt.Fprintln(out);fmt.Fprintln(out)};fmt.Fprint(out,pigs(b,d,t))}}
