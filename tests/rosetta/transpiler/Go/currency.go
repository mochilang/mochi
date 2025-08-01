//go:build ignore

// Generated by Mochi v0.10.42 on 2025-07-27 16:25:56 GMT+7
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

func parseIntDigits(s string) int {
	var n int = 0
	var i int = 0
	var digits map[string]int = map[string]int{"0": 0, "1": 1, "2": 2, "3": 3, "4": 4, "5": 5, "6": 6, "7": 7, "8": 8, "9": 9}
	for i < len(s) {
		var ch string = string([]rune(s)[i:(i + 1)])
		if !func() bool { _, ok := digits[ch]; return ok }() {
			return 0
		}
		n = ((n * 10) + digits[ch])
		i = (i + 1)
	}
	return n
}

func parseDC(s string) int {
	var neg bool = false
	if (len(s) > 0) && (string([]rune(s)[0:1]) == "-") {
		neg = true
		s = _substr(s, 1, len(s))
	}
	var dollars int = 0
	var cents int = 0
	var i int = 0
	var seenDot bool = false
	var centDigits int = 0
	for i < len(s) {
		var ch string = string([]rune(s)[i:(i + 1)])
		if ch == "." {
			seenDot = true
			i = (i + 1)
			continue
		}
		var d int = parseIntDigits(ch)
		if seenDot {
			if centDigits < 2 {
				cents = ((cents * 10) + d)
				centDigits = (centDigits + 1)
			}
		} else {
			dollars = ((dollars * 10) + d)
		}
		i = (i + 1)
	}
	if centDigits == 1 {
		cents = (cents * 10)
	}
	var val int = ((dollars * 100) + cents)
	if neg {
		val = (0 - val)
	}
	return val
}

func parseRate(s string) int {
	var neg bool = false
	if (len(s) > 0) && (string([]rune(s)[0:1]) == "-") {
		neg = true
		s = _substr(s, 1, len(s))
	}
	var whole int = 0
	var frac int = 0
	var digits int = 0
	var seenDot bool = false
	var i int = 0
	for i < len(s) {
		var ch string = string([]rune(s)[i:(i + 1)])
		if ch == "." {
			seenDot = true
			i = (i + 1)
			continue
		}
		var d int = parseIntDigits(ch)
		if seenDot {
			if digits < 4 {
				frac = ((frac * 10) + d)
				digits = (digits + 1)
			}
		} else {
			whole = ((whole * 10) + d)
		}
		i = (i + 1)
	}
	for digits < 4 {
		frac = (frac * 10)
		digits = (digits + 1)
	}
	var val int = ((whole * 10000) + frac)
	if neg {
		val = (0 - val)
	}
	return val
}

func dcString(dc int) string {
	var d int = (dc / 100)
	var n int = dc
	if n < 0 {
		n = (0 - n)
	}
	var c int = (n % 100)
	var cstr string = fmt.Sprint(c)
	if len(cstr) == 1 {
		cstr = ("0" + cstr)
	}
	return ((fmt.Sprint(d) + ".") + cstr)
}

func extend(dc int, n int) int {
	return (dc * n)
}

func tax(total int, rate int) int {
	return int((((total * rate) + 5000) / 10000))
}

func padLeft(s string, n int) string {
	var out string = s
	for len(out) < n {
		out = (" " + out)
	}
	return out
}

func mochiMain() {
	var hp int = parseDC("5.50")
	var mp int = parseDC("2.86")
	var rate int = parseRate("0.0765")
	var totalBeforeTax int = (extend(hp, 4000000000000000) + extend(mp, 2))
	var t int = tax(totalBeforeTax, rate)
	var total int = (totalBeforeTax + t)
	fmt.Println(("Total before tax: " + padLeft(dcString(totalBeforeTax), 22)))
	fmt.Println(("             Tax: " + padLeft(dcString(t), 22)))
	fmt.Println(("           Total: " + padLeft(dcString(total), 22)))
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
