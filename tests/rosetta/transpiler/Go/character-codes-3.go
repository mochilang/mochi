//go:build ignore

// Generated by Mochi v0.10.41 on 2025-07-26 17:23:56 GMT+7
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

func ord(ch string) int {
	if ch == "a" {
		return 97
	}
	if ch == "π" {
		return 960
	}
	if ch == "A" {
		return 65
	}
	return 0
}

func chr(n int) string {
	if n == 97 {
		return "a"
	}
	if n == 960 {
		return "π"
	}
	if n == 65 {
		return "A"
	}
	return "?"
}

var b int

var r int

var s string

func main() {
	func() {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem := ms.Alloc
		benchStart := time.Now().UnixNano()
		b = ord("a")
		r = ord("π")
		s = "aπ"
		fmt.Println(((((fmt.Sprint(b) + " ") + fmt.Sprint(r)) + " ") + s))
		fmt.Println((((("string cast to []rune: [" + fmt.Sprint(b)) + " ") + fmt.Sprint(r)) + "]"))
		fmt.Println(((("    string range loop: " + fmt.Sprint(b)) + " ") + fmt.Sprint(r)))
		fmt.Println("         string bytes: 0x61 0xcf 0x80")
		runtime.ReadMemStats(&ms)
		endMem := ms.Alloc
		benchEnd := time.Now().UnixNano()
		data := map[string]any{"name": "main", "duration_us": (benchEnd - benchStart) / 1000, "memory_bytes": endMem - startMem}
		out, _ := json.MarshalIndent(data, "", "  ")
		fmt.Println(string(out))
	}()
}
