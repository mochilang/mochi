//go:build ignore

// Generated by Mochi v0.10.52 on 2025-08-01 21:27:53 GMT+7
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

func pow(base int, exp int) int {
	var result int = 1
	_ = result
	var i int = 0
	_ = i
	for i < exp {
		result = (result * base)
		i = (i + 1)
	}
	return result
}

func ackermann2(m int, n int) int {
	if m == 0 {
		return (n + 1)
	}
	if m == 1 {
		return (n + 2)
	}
	if m == 2 {
		return ((2 * n) + 3)
	}
	if m == 3 {
		return ((8 * pow(2, n)) - 3)
	}
	if n == 0 {
		return ackermann2((m - 1), 1)
	}
	return ackermann2((m - 1), ackermann2(m, (n-1)))
}

func mochiMain() {
	fmt.Println(("A(0, 0) = " + fmt.Sprint(ackermann2(0, 0))))
	fmt.Println(("A(1, 2) = " + fmt.Sprint(ackermann2(1, 2))))
	fmt.Println(("A(2, 4) = " + fmt.Sprint(ackermann2(2, 4))))
	fmt.Println(("A(3, 4) = " + fmt.Sprint(ackermann2(3, 4))))
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
