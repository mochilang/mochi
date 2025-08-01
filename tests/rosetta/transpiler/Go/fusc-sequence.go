//go:build ignore

// Generated by Mochi v0.10.55 on 2025-08-02 18:07:04 GMT+7
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

func _substr(s string, start, end int) string {
	r := []rune(s)
	if start < 0 {
		start = 0
	}
	if end > len(r) {
		end = len(r)
	}
	if start > len(r) {
		start = len(r)
	}
	if end < start {
		end = start
	}
	return string(r[start:end])
}

func fuscVal(n int) int {
	var a int = 1
	_ = a
	var b int = 0
	_ = b
	var x int = n
	_ = x
	for x > 0 {
		if (x % 2) == 0 {
			x = (x / 2)
			a = (a + b)
		} else {
			x = ((x - 1) / 2)
			b = (a + b)
		}
	}
	if n == 0 {
		return 0
	}
	return b
}

func firstFusc(n int) []int {
	var arr []int = []int{}
	_ = arr
	var i int = 0
	_ = i
	for i < n {
		arr = append(arr, fuscVal(i))
		i = (i + 1)
	}
	return arr
}

func commatize(n int) string {
	var s string = fmt.Sprint(n)
	_ = s
	var neg bool = false
	_ = neg
	if n < 0 {
		neg = true
		s = _substr(s, 1, len(s))
	}
	var i int = (len(s) - 3)
	_ = i
	for i >= 1 {
		s = ((_substr(s, 0, i) + ",") + _substr(s, i, len(s)))
		i = (i - 3)
	}
	if neg {
		return ("-" + s)
	}
	return s
}

func padLeft(s string, w int) string {
	var out string = s
	_ = out
	for len(out) < w {
		out = (" " + out)
	}
	return out
}

func mochiMain() {
	fmt.Println("The first 61 fusc numbers are:")
	fmt.Println(fmt.Sprint(firstFusc(61)))
	fmt.Println("\nThe fusc numbers whose length > any previous fusc number length are:")
	var idxs []int = []int{0, 37, 1173, 35499, 699051, 19573419}
	_ = idxs
	var i int = 0
	_ = i
	for i < len(idxs) {
		var idx int = idxs[i]
		_ = idx
		var val int = fuscVal(idx)
		_ = val
		var numStr string = padLeft(commatize(val), 7)
		_ = numStr
		var idxStr string = padLeft(commatize(idx), 10)
		_ = idxStr
		fmt.Println((((numStr + " (index ") + idxStr) + ")"))
		i = (i + 1)
	}
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
