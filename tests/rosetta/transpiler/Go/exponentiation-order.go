//go:build ignore

// Generated by Mochi v0.10.42 on 2025-07-27 23:54:42 GMT+7
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

func powInt(b int, p int) int {
	var r int = 1
	_ = r
	var i int = 0
	_ = i
	for i < p {
		r = (r * b)
		i = (i + 1)
	}
	return r
}

var a int

var b int

var c int

func main() {
	func() {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem := ms.Alloc
		benchStart := time.Now().UnixNano()
		a = powInt(5, powInt(3, 2))
		b = powInt(powInt(5, 3), 2)
		c = powInt(5, powInt(3, 2))
		fmt.Println(("5^3^2   = " + fmt.Sprint(a)))
		fmt.Println(("(5^3)^2 = " + fmt.Sprint(b)))
		fmt.Println(("5^(3^2) = " + fmt.Sprint(c)))
		runtime.ReadMemStats(&ms)
		endMem := ms.Alloc
		benchEnd := time.Now().UnixNano()
		data := map[string]any{"name": "main", "duration_us": (benchEnd - benchStart) / 1000, "memory_bytes": endMem - startMem}
		out, _ := json.MarshalIndent(data, "", "  ")
		fmt.Println(string(out))
	}()
}
