//go:build ignore

// Generated by Mochi v0.10.54 on 2025-08-02 14:40:47 GMT+7
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

func show(n int) {
	if n == 1 {
		fmt.Println("1: 1")
		return
	}
	var out string = (fmt.Sprint(n) + ": ")
	_ = out
	var x string = ""
	_ = x
	var m int = n
	_ = m
	var f int = 2
	_ = f
	for m != 1 {
		if (m % f) == 0 {
			out = ((out + x) + fmt.Sprint(f))
			x = "×"
			m = int((m / f))
		} else {
			f = (f + 1)
		}
	}
	fmt.Println(out)
}

func main() {
	func() {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem := ms.Alloc
		benchStart := time.Now().UnixNano()
		show(1)
		for i := 2; i < 10; i++ {
			show(i)
		}
		fmt.Println("...")
		for i := 2144; i < 2155; i++ {
			show(i)
		}
		fmt.Println("...")
		for i := 9987; i < 10000; i++ {
			show(i)
		}
		runtime.ReadMemStats(&ms)
		endMem := ms.Alloc
		benchEnd := time.Now().UnixNano()
		data := map[string]any{"name": "main", "duration_us": (benchEnd - benchStart) / 1000, "memory_bytes": endMem - startMem}
		out, _ := json.MarshalIndent(data, "", "  ")
		fmt.Println(string(out))
	}()
}
