//go:build ignore

// Generated by Mochi v0.10.40 on 2025-07-26 11:15:58 GMT+7
package main

import (
	"encoding/json"
	"fmt"
	"os"
	"runtime"
	"strconv"
	"time"
)

var seededNow bool
var nowSeed int64

func init() {
	if s := os.Getenv("MOCHI_NOW_SEED"); s != "" {
		if v, err := strconv.ParseInt(s, 10, 64); err == nil {
			nowSeed = v
			seededNow = true
		}
	}
}
func _now() int {
	if seededNow {
		nowSeed = (nowSeed*1664525 + 1013904223) % 2147483647
		return int(nowSeed)
	}
	return int(time.Now().UnixNano())
}

func listStr(xs []int) string {
	var s string = "["
	_ = s
	var i int = 0
	_ = i
	for i < len(xs) {
		s = (s + fmt.Sprint(xs[i]))
		if i < (len(xs) - 1) {
			s = (s + " ")
		}
		i = (i + 1)
	}
	s = (s + "]")
	return s
}

func llStr(lst [][]int) string {
	var s string = "["
	_ = s
	var i int = 0
	_ = i
	for i < len(lst) {
		s = (s + listStr(lst[i]))
		if i < (len(lst) - 1) {
			s = (s + " ")
		}
		i = (i + 1)
	}
	s = (s + "]")
	return s
}

func concat(a []int, b []int) []int {
	var out []int = []int{}
	_ = out
	for _, v := range a {
		out = append(out, v)
	}
	for _, v := range b {
		out = append(out, v)
	}
	return out
}

func cartN(lists any) [][]int {
	if lists == nil {
		return [][]int{}
	}
	var a [][]int = func(v any) [][]int {
		if vv, ok := v.([][]int); ok {
			return vv
		}
		if arr, ok := v.([]any); ok && len(arr) == 0 {
			return [][]int{}
		}
		return v.([][]int)
	}(lists)
	_ = a
	if len(a) == 0 {
		return [][]int{[]int{}}
	}
	var out [][]int = [][]int{}
	_ = out
	var rest [][]int = cartN(a[1:len(a)])
	_ = rest
	for _, x := range a[0] {
		for _, p := range rest {
			out = append(out, concat([]int{x}, p))
		}
	}
	return out
}

func mochiMain() {
	fmt.Println(llStr(cartN([][]int{[]int{1, 2}, []int{3, 4}})))
	fmt.Println(llStr(cartN([][]int{[]int{3, 4}, []int{1, 2}})))
	fmt.Println(llStr(cartN([][]int{[]int{1, 2}, []int{}})))
	fmt.Println(llStr(cartN([][]int{[]int{}, []int{1, 2}})))
	fmt.Println("")
	fmt.Println("[")
	for _, p := range cartN([][]int{[]int{1776, 1789}, []int{7, 12}, []int{4, 14, 23}, []int{0, 1}}) {
		fmt.Println((" " + listStr(p)))
	}
	fmt.Println("]")
	fmt.Println(llStr(cartN([][]int{[]int{1, 2, 3}, []int{30}, []int{500, 100}})))
	fmt.Println(llStr(cartN([][]int{[]int{1, 2, 3}, []int{}, []int{500, 100}})))
	fmt.Println("")
	fmt.Println(llStr(cartN(nil)))
	fmt.Println(llStr(cartN([]any{})))
}

func main() {
	func() {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem := ms.Alloc
		benchStart := time.Now().UnixNano()
		mochiMain()
		runtime.ReadMemStats(&ms)
		endMem := ms.Alloc
		benchEnd := time.Now().UnixNano()
		data := map[string]any{"name": "main", "duration_us": (benchEnd - benchStart) / 1000, "memory_bytes": endMem - startMem}
		out, _ := json.MarshalIndent(data, "", "  ")
		fmt.Println(string(out))
	}()
}
