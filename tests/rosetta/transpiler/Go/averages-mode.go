//go:build ignore

// Generated by Mochi v0.10.52 on 2025-08-02 01:03:32 GMT+7
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

var arr1 []int

var counts1 map[int]int

var keys1 []int

var i int

var max1 int

var modes1 []int

var arr2 []int

var counts2 map[int]int

var keys2 []int

var max2 int

var modes2 []int

func main() {
	func() {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem := ms.Alloc
		benchStart := time.Now().UnixNano()
		arr1 = []int{2, 7, 1, 8, 2}
		counts1 = map[int]int{}
		keys1 = []int{}
		i = 0
		for i < len(arr1) {
			var v int = arr1[i]
			_ = v
			if func() bool { _, ok := counts1[v]; return ok }() {
				counts1[v] = (counts1[v] + 1)
			} else {
				counts1[v] = 1
				keys1 = append(keys1, v)
			}
			i = (i + 1)
		}
		max1 = 0
		i = 0
		for i < len(keys1) {
			var k int = keys1[i]
			_ = k
			var c int = counts1[k]
			_ = c
			if c > max1 {
				max1 = c
			}
			i = (i + 1)
		}
		modes1 = []int{}
		i = 0
		for i < len(keys1) {
			var k int = keys1[i]
			_ = k
			if counts1[k] == max1 {
				modes1 = append(modes1, k)
			}
			i = (i + 1)
		}
		fmt.Println(fmt.Sprint(modes1))
		arr2 = []int{2, 7, 1, 8, 2, 8}
		counts2 = map[int]int{}
		keys2 = []int{}
		i = 0
		for i < len(arr2) {
			var v int = arr2[i]
			_ = v
			if func() bool { _, ok := counts2[v]; return ok }() {
				counts2[v] = (counts2[v] + 1)
			} else {
				counts2[v] = 1
				keys2 = append(keys2, v)
			}
			i = (i + 1)
		}
		max2 = 0
		i = 0
		for i < len(keys2) {
			var k int = keys2[i]
			_ = k
			var c int = counts2[k]
			_ = c
			if c > max2 {
				max2 = c
			}
			i = (i + 1)
		}
		modes2 = []int{}
		i = 0
		for i < len(keys2) {
			var k int = keys2[i]
			_ = k
			if counts2[k] == max2 {
				modes2 = append(modes2, k)
			}
			i = (i + 1)
		}
		fmt.Println(fmt.Sprint(modes2))
		runtime.ReadMemStats(&ms)
		endMem := ms.Alloc
		benchEnd := time.Now().UnixNano()
		data := map[string]any{"name": "main", "duration_us": (benchEnd - benchStart) / 1000, "memory_bytes": endMem - startMem}
		out, _ := json.MarshalIndent(data, "", "  ")
		fmt.Println(string(out))
	}()
}
