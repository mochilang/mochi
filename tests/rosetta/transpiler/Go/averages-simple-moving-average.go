//go:build ignore

// Generated by Mochi v0.10.40 on 2025-07-25 20:04:00 GMT+7
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

func indexOf(s string, ch string) int {
	var i int = 0
	_ = i
	for i < len(s) {
		if _substr(s, i, (i+1)) == ch {
			return i
		}
		i = (i + 1)
	}
	return (0 - 1)
}

func fmt3(x float64) string {
	var y float64 = (float64(int(((x * 1000.0) + 0.5))) / 1000.0)
	_ = y
	var s string = fmt.Sprint(y)
	_ = s
	var dot int = indexOf(s, ".")
	_ = dot
	if dot == (0 - 1) {
		s = (s + ".000")
	} else {
		var decs int = ((len(s) - dot) - 1)
		_ = decs
		if decs > 3 {
			s = _substr(s, 0, (dot + 4))
		} else {
			for decs < 3 {
				s = (s + "0")
				decs = (decs + 1)
			}
		}
	}
	return s
}

func pad(s string, width int) string {
	var out string = s
	_ = out
	for len(out) < width {
		out = (" " + out)
	}
	return out
}

func smaSeries(xs []float64, period int) []float64 {
	var res []float64 = []float64{}
	_ = res
	var sum float64 = 0.0
	_ = sum
	var i int = 0
	_ = i
	for i < len(xs) {
		sum = (sum + xs[i])
		if i >= period {
			sum = (sum - xs[(i-period)])
		}
		var denom int = (i + 1)
		_ = denom
		if denom > period {
			denom = period
		}
		res = append(res, (sum / float64(denom)))
		i = (i + 1)
	}
	return res
}

func mochiMain() {
	var xs []float64 = []float64{1.0, 2.0, 3.0, 4.0, 5.0, 5.0, 4.0, 3.0, 2.0, 1.0}
	_ = xs
	var sma3 []float64 = smaSeries(xs, 3)
	_ = sma3
	var sma5 []float64 = smaSeries(xs, 5)
	_ = sma5
	fmt.Println("x       sma3   sma5")
	var i int = 0
	_ = i
	for i < len(xs) {
		var line string = ((((pad(fmt3(xs[i]), 5) + "  ") + pad(fmt3(sma3[i]), 5)) + "  ") + pad(fmt3(sma5[i]), 5))
		_ = line
		fmt.Println(line)
		i = (i + 1)
	}
}

func main() {
	func() {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem := ms.Alloc
		start := _now()
		mochiMain()
		runtime.ReadMemStats(&ms)
		endMem := ms.Alloc
		end := _now()
		data := map[string]any{"name": "main", "duration_us": (end - start) / 1000, "memory_bytes": endMem - startMem}
		out, _ := json.MarshalIndent(data, "", "  ")
		fmt.Println(string(out))
	}()
}
