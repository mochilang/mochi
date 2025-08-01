//go:build ignore

// Generated by Mochi v0.10.52 on 2025-08-01 21:30:59 GMT+7
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

func isPrime(n int) bool {
	if n < 2 {
		return false
	}
	if (n % 2) == 0 {
		return (n == 2)
	}
	if (n % 3) == 0 {
		return (n == 3)
	}
	var d int = 5
	_ = d
	for (d * d) <= n {
		if (n % d) == 0 {
			return false
		}
		d = (d + 2)
		if (n % d) == 0 {
			return false
		}
		d = (d + 4)
	}
	return true
}

func sumDigits(n int) int {
	var s int = 0
	_ = s
	var x int = n
	_ = x
	for x > 0 {
		s = (s + (x % 10))
		x = int((x / 10))
	}
	return s
}

func pad(n int) string {
	if n < 10 {
		return ("  " + fmt.Sprint(n))
	}
	if n < 100 {
		return (" " + fmt.Sprint(n))
	}
	return fmt.Sprint(n)
}

func mochiMain() {
	fmt.Println("Additive primes less than 500:")
	var count int = 0
	_ = count
	var line string = ""
	_ = line
	var lineCount int = 0
	_ = lineCount
	var i int = 2
	_ = i
	for i < 500 {
		if isPrime(i) && isPrime(sumDigits(i)) {
			count = (count + 1)
			line = ((line + pad(i)) + "  ")
			lineCount = (lineCount + 1)
			if lineCount == 10 {
				fmt.Println(_substr(line, 0, (len(line) - 2)))
				line = ""
				lineCount = 0
			}
		}
		if i > 2 {
			i = (i + 2)
		} else {
			i = (i + 1)
		}
	}
	if lineCount > 0 {
		fmt.Println(_substr(line, 0, (len(line) - 2)))
	}
	fmt.Println((fmt.Sprint(count) + " additive primes found."))
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
