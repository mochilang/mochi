package main
import("bufio";"fmt";"os")
func rearrange(s string,k int)string{if k<=1{return s};cnt:=make([]int,26);last:=make([]int,26);for i:=range last{last[i]=-1<<30};for _,ch:=range s{cnt[ch-'a']++};out:=make([]byte,0,len(s));for pos:=0;pos<len(s);pos++{best:=-1;for i:=0;i<26;i++{if cnt[i]>0&&pos-last[i]>=k&&(best<0||cnt[i]>cnt[best]){best=i}};if best<0{return ""};out=append(out,byte('a'+best));cnt[best]--;last[best]=pos};return string(out)}
func main(){in:=bufio.NewReader(os.Stdin);out:=bufio.NewWriter(os.Stdout);defer out.Flush();var t int;if _,err:=fmt.Fscan(in,&t);err!=nil{return};for tc:=0;tc<t;tc++{var s string;var k int;fmt.Fscan(in,&s,&k);if tc>0{fmt.Fprintln(out);fmt.Fprintln(out)};fmt.Fprintf(out,"\"%s\"",rearrange(s,k))}}
