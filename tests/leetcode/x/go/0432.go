package main
import("bufio";"fmt";"os")
type AllOne struct{m map[string]int}
func New()*AllOne{return &AllOne{map[string]int{}}}
func(a*AllOne)Inc(k string){a.m[k]++}
func(a*AllOne)Dec(k string){a.m[k]--;if a.m[k]==0{delete(a.m,k)}}
func(a*AllOne)Max()string{if len(a.m)==0{return ""};best:="";val:=-1;for k,v:=range a.m{if v>val||v==val&&(best==""||k<best){best=k;val=v}};return best}
func(a*AllOne)Min()string{if len(a.m)==0{return ""};best:="";val:=1<<30;for k,v:=range a.m{if v<val||v==val&&(best==""||k<best){best=k;val=v}};return best}
func q(s string,isNull bool)string{if isNull{return "null"};return "\""+s+"\""}
func main(){in:=bufio.NewReader(os.Stdin);out:=bufio.NewWriter(os.Stdout);defer out.Flush();var t int;if _,e:=fmt.Fscan(in,&t);e!=nil{return};for tc:=0;tc<t;tc++{var n int;fmt.Fscan(in,&n);var a *AllOne;res:=[]string{};for i:=0;i<n;i++{var op string;fmt.Fscan(in,&op);switch op{case "C":a=New();res=append(res,q("",true));case "I":var k string;fmt.Fscan(in,&k);a.Inc(k);res=append(res,q("",true));case "D":var k string;fmt.Fscan(in,&k);a.Dec(k);res=append(res,q("",true));case "X":res=append(res,q(a.Max(),false));case "N":res=append(res,q(a.Min(),false))}};if tc>0{fmt.Fprintln(out);fmt.Fprintln(out)};fmt.Fprint(out,"[");for i,s:=range res{if i>0{fmt.Fprint(out,",")};fmt.Fprint(out,s)};fmt.Fprint(out,"]")}}
