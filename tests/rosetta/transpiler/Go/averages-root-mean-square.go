//go:build ignore

// Generated by Mochi v0.10.52 on 2025-08-02 01:03:35 GMT+7
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

func sqrtApprox(x float64) float64 {
	var guess float64 = x
	_ = guess
	var i int = 0
	_ = i
	for i < 20 {
		guess = ((guess + (x / guess)) / 2.0)
		i = (i + 1)
	}
	return guess
}

var n int

var sum float64

var x int

var rms float64

func main() {
	func() {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem := ms.Alloc
		benchStart := time.Now().UnixNano()
		n = 10
		sum = 0.0
		x = 1
		for x <= n {
			sum = (sum + (float64(x) * float64(x)))
			x = (x + 1)
		}
		rms = sqrtApprox((sum / float64(n)))
		fmt.Println(fmt.Sprint(rms))
		runtime.ReadMemStats(&ms)
		endMem := ms.Alloc
		benchEnd := time.Now().UnixNano()
		data := map[string]any{"name": "main", "duration_us": (benchEnd - benchStart) / 1000, "memory_bytes": endMem - startMem}
		out, _ := json.MarshalIndent(data, "", "  ")
		fmt.Println(string(out))
	}()
}
