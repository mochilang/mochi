//go:build ignore

// Generated by Mochi v0.10.55 on 2025-08-02 18:06:44 GMT+7
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

func pow10(n int) float64 {
	var r float64 = 1.0
	_ = r
	var i int = 0
	_ = i
	for i < n {
		r = (r * 10.0)
		i = (i + 1)
	}
	return r
}

func formatFloat(f float64, prec int) string {
	var scale float64 = pow10(prec)
	_ = scale
	var scaled float64 = ((f * scale) + 0.5)
	_ = scaled
	_ = scaled
	var n int = int(scaled)
	_ = n
	var digits string = fmt.Sprint(n)
	_ = digits
	for len(digits) <= prec {
		digits = ("0" + digits)
	}
	var intPart string = _substr(digits, 0, (len(digits) - prec))
	_ = intPart
	_ = intPart
	var fracPart string = _substr(digits, (len(digits) - prec), len(digits))
	_ = fracPart
	_ = fracPart
	return ((intPart + ".") + fracPart)
}

func padLeftZeros(s string, width int) string {
	var out string = s
	_ = out
	for len(out) < width {
		out = ("0" + out)
	}
	return out
}

func main() {
	func() {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem := ms.Alloc
		benchStart := time.Now().UnixNano()
		fmt.Println(padLeftZeros(formatFloat(7.125, 3), 9))
		runtime.ReadMemStats(&ms)
		endMem := ms.Alloc
		benchEnd := time.Now().UnixNano()
		data := map[string]any{"name": "main", "duration_us": (benchEnd - benchStart) / 1000, "memory_bytes": endMem - startMem}
		out, _ := json.MarshalIndent(data, "", "  ")
		fmt.Println(string(out))
	}()
}
