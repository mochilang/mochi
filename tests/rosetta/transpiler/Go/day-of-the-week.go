//go:build ignore

// Generated by Mochi v0.10.42 on 2025-07-27 17:04:05 GMT+7
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

func weekday(y int, m int, d int) int {
	var yy int = y
	var mm int = m
	if mm < 3 {
		mm = (mm + 12)
		yy = (yy - 1)
	}
	var k int = (yy % 100)
	var j int = int((yy / 100))
	var a int = int(((13 * (mm + 1)) / 5))
	var b int = int((k / 4))
	var c int = int((j / 4))
	return ((((((d + a) + k) + b) + c) + (5 * j)) % 7)
}

func main() {
	func() {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem := ms.Alloc
		benchStart := time.Now().UnixNano()
		for year := 2008; year < 2122; year++ {
			if weekday(year, 12, 25) == 1 {
				fmt.Println((("25 December " + fmt.Sprint(year)) + " is Sunday"))
			}
		}
		runtime.ReadMemStats(&ms)
		endMem := ms.Alloc
		benchEnd := time.Now().UnixNano()
		data := map[string]any{"name": "main", "duration_us": (benchEnd - benchStart) / 1000, "memory_bytes": endMem - startMem}
		out, _ := json.MarshalIndent(data, "", "  ")
		fmt.Println(string(out))
	}()
}
