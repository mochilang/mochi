package main
import("bufio";"fmt";"os";"strings")
type RC struct{vals []int; pos map[int]map[int]bool}
func New()*RC{return &RC{pos:map[int]map[int]bool{}}}
func (r *RC)Ins(v int)bool{fresh:=len(r.pos[v])==0;if r.pos[v]==nil{r.pos[v]=map[int]bool{}};r.pos[v][len(r.vals)]=true;r.vals=append(r.vals,v);return fresh}
func (r *RC)Rem(v int)bool{if len(r.pos[v])==0{return false};idx:=-1;for i:=range r.pos[v]{if i>idx{idx=i}};delete(r.pos[v],idx);last:=r.vals[len(r.vals)-1];li:=len(r.vals)-1;if idx!=li{r.vals[idx]=last;delete(r.pos[last],li);r.pos[last][idx]=true};r.vals=r.vals[:li];return true}
func main(){in:=bufio.NewReader(os.Stdin);out:=bufio.NewWriter(os.Stdout);defer out.Flush();var t int;if _,e:=fmt.Fscan(in,&t);e!=nil{return};cases:=[]string{};for ;t>0;t--{var ops int;fmt.Fscan(in,&ops);var rc *RC;res:=[]string{};for i:=0;i<ops;i++{var op string;fmt.Fscan(in,&op);switch op{case"C":rc=New();res=append(res,"null");case"I":var v int;fmt.Fscan(in,&v);if rc.Ins(v){res=append(res,"true")}else{res=append(res,"false")};case"R":var v int;fmt.Fscan(in,&v);if rc.Rem(v){res=append(res,"true")}else{res=append(res,"false")};case"G":res=append(res,fmt.Sprint(rc.vals[0]))}};cases=append(cases,"["+strings.Join(res,",")+"]")};fmt.Fprint(out,strings.Join(cases,"\n\n"))}
