//go:build ignore

// Generated by Mochi v0.10.54 on 2025-08-02 14:40:38 GMT+7
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

func gcd(a int, b int) int {
	var x int = a
	_ = x
	if x < 0 {
		x = (0 - x)
	}
	var y int = b
	_ = y
	if y < 0 {
		y = (0 - y)
	}
	for y != 0 {
		var t int = (x % y)
		_ = t
		x = y
		y = t
	}
	return x
}

func parseRational(s string) map[string]int {
	var intPart int = 0
	_ = intPart
	var fracPart int = 0
	_ = fracPart
	var denom int = 1
	_ = denom
	var afterDot bool = false
	_ = afterDot
	var i int = 0
	_ = i
	for i < len(s) {
		var ch string = _substr(s, i, (i + 1))
		_ = ch
		if ch == "." {
			afterDot = true
		} else {
			var d int = (func() int { n, _ := strconv.Atoi(ch); return n }() - func() int { n, _ := strconv.Atoi("0"); return n }())
			_ = d
			if !afterDot {
				intPart = ((intPart * 10) + d)
			} else {
				fracPart = ((fracPart * 10) + d)
				denom = (denom * 10)
			}
		}
		i = (i + 1)
	}
	var num int = ((intPart * denom) + fracPart)
	_ = num
	var g int = gcd(num, denom)
	_ = g
	_ = g
	return func(v any) map[string]int {
		if v == nil {
			return map[string]int{}
		}
		if vv, ok := v.(map[string]int); ok {
			return vv
		}
		out := make(map[string]int)
		if m, ok := v.(map[string]any); ok {
			for k, vv := range m {
				if vi, ok2 := vv.(int); ok2 {
					out[k] = vi
				}
			}
		}
		return out
	}(map[string]int{"num": int((num / g)), "den": int((denom / g))})
}

func mochiMain() {
	var inputs []string = []string{"0.9054054", "0.518518", "0.75"}
	_ = inputs
	for _, s := range inputs {
		var r map[string]int = parseRational(s)
		_ = r
		fmt.Println(((((s + " = ") + fmt.Sprint(r["num"])) + "/") + fmt.Sprint(r["den"])))
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
