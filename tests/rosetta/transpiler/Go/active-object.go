//go:build ignore

// Generated by Mochi v0.10.52 on 2025-08-01 21:30:09 GMT+7
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

var PI float64

func sinApprox(x float64) float64 {
	var term float64 = x
	_ = term
	var sum float64 = x
	_ = sum
	var n int = 1
	_ = n
	for n <= 12 {
		var denom float64 = float64(((2 * n) * ((2 * n) + 1)))
		_ = denom
		term = ((((0 - term) * x) * x) / denom)
		sum = (sum + term)
		n = (n + 1)
	}
	return sum
}

var dt float64

var s float64

var t1 float64

var k1 float64

var i int

var i2 int

func main() {
	func() {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem := ms.Alloc
		benchStart := time.Now().UnixNano()
		PI = 3.141592653589793
		dt = 0.01
		s = 0.0
		t1 = 0.0
		k1 = sinApprox(0.0)
		i = 1
		for i <= 200 {
			var t2 float64 = (float64(i) * dt)
			_ = t2
			var k2 float64 = sinApprox((t2 * PI))
			_ = k2
			s = (s + (((k1 + k2) * 0.5) * (t2 - t1)))
			t1 = t2
			k1 = k2
			i = (i + 1)
		}
		i2 = 1
		for i2 <= 50 {
			var t2 float64 = (2.0 + (float64(i2) * dt))
			_ = t2
			var k2 float64 = 0.0
			_ = k2
			s = (s + (((k1 + k2) * 0.5) * (t2 - t1)))
			t1 = t2
			k1 = k2
			i2 = (i2 + 1)
		}
		fmt.Println(func() string {
			f := float64(s)
			if f == float64(int(f)) {
				return fmt.Sprintf("%.1f", f)
			}
			return fmt.Sprint(f)
		}())
		runtime.ReadMemStats(&ms)
		endMem := ms.Alloc
		benchEnd := time.Now().UnixNano()
		data := map[string]any{"name": "main", "duration_us": (benchEnd - benchStart) / 1000, "memory_bytes": endMem - startMem}
		out, _ := json.MarshalIndent(data, "", "  ")
		fmt.Println(string(out))
	}()
}
