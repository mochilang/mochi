package main
import("bufio";"fmt";"os")
type P struct{x,y int}
func toggle(s map[P]bool,p P){if s[p]{delete(s,p)}else{s[p]=true}}
func cover(rects [][4]int)bool{minx,miny,maxx,maxy:=1<<60,1<<60,-1<<60,-1<<60;area:=0;corners:=map[P]bool{};for _,r:=range rects{x1,y1,x2,y2:=r[0],r[1],r[2],r[3];if x1<minx{minx=x1};if y1<miny{miny=y1};if x2>maxx{maxx=x2};if y2>maxy{maxy=y2};area+=(x2-x1)*(y2-y1);toggle(corners,P{x1,y1});toggle(corners,P{x1,y2});toggle(corners,P{x2,y1});toggle(corners,P{x2,y2})};outer:=map[P]bool{{minx,miny}:true,{minx,maxy}:true,{maxx,miny}:true,{maxx,maxy}:true};if area!=(maxx-minx)*(maxy-miny)||len(corners)!=4{return false};for p:=range corners{if !outer[p]{return false}};return true}
func main(){in:=bufio.NewReader(os.Stdin);out:=bufio.NewWriter(os.Stdout);defer out.Flush();var t int;if _,e:=fmt.Fscan(in,&t);e!=nil{return};for tc:=0;tc<t;tc++{var n int;fmt.Fscan(in,&n);rs:=make([][4]int,n);for i:=range rs{fmt.Fscan(in,&rs[i][0],&rs[i][1],&rs[i][2],&rs[i][3])};if tc>0{fmt.Fprintln(out);fmt.Fprintln(out)};if cover(rs){fmt.Fprint(out,"true")}else{fmt.Fprint(out,"false")}}}
