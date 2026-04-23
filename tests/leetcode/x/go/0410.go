package main
import("bufio";"fmt";"os")
func splitArray(nums []int,k int)int{lo,hi:=0,0;for _,x:=range nums{if x>lo{lo=x};hi+=x};for lo<hi{mid:=(lo+hi)/2;pieces,cur:=1,0;for _,x:=range nums{if cur+x>mid{pieces++;cur=x}else{cur+=x}};if pieces<=k{hi=mid}else{lo=mid+1}};return lo}
func main(){in:=bufio.NewReader(os.Stdin);out:=bufio.NewWriter(os.Stdout);defer out.Flush();var t int;if _,e:=fmt.Fscan(in,&t);e!=nil{return};for tc:=0;tc<t;tc++{var n,k int;fmt.Fscan(in,&n,&k);nums:=make([]int,n);for i:=range nums{fmt.Fscan(in,&nums[i])};if tc>0{fmt.Fprintln(out);fmt.Fprintln(out)};fmt.Fprint(out,splitArray(nums,k))}}
