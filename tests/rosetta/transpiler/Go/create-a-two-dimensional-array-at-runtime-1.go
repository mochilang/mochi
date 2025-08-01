//go:build ignore

// Generated by Mochi v0.10.54 on 2025-08-02 14:41:04 GMT+7
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

func mochiMain() {
	var row int = 3
	_ = row
	var col int = 4
	_ = col
	var a [][]int = [][]int{}
	_ = a
	var i int = 0
	_ = i
	for i < row {
		var rowArr []int = []int{}
		_ = rowArr
		var j int = 0
		_ = j
		for j < col {
			rowArr = append(rowArr, 0)
			j = (j + 1)
		}
		a = append(a, rowArr)
		i = (i + 1)
	}
	fmt.Println(("a[0][0] = " + fmt.Sprint(a[0][0])))
	a[int((row - 1))][int((col - 1))] = 7
	fmt.Println(((((("a[" + fmt.Sprint((row - 1))) + "][") + fmt.Sprint((col - 1))) + "] = ") + fmt.Sprint(a[int((row - 1))][int((col-1))])))
	a = nil
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
