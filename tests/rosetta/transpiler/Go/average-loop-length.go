//go:build ignore

// Generated by Mochi v0.10.40 on 2025-07-25 20:01:53 GMT+7
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

func absf(x float64) float64 {
	if x < 0.0 {
		return (0 - x)
	}
	return x
}

func floorf(x float64) float64 {
	var y int = int(x)
	_ = y
	return float64(y)
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

func fmtF(x float64) string {
	var y float64 = (floorf(((x * 10000.0) + 0.5)) / 10000.0)
	_ = y
	var s string = fmt.Sprint(y)
	_ = s
	var dot int = indexOf(s, ".")
	_ = dot
	if dot == (0 - 1) {
		s = (s + ".0000")
	} else {
		var decs int = ((len(s) - dot) - 1)
		_ = decs
		if decs > 4 {
			s = _substr(s, 0, (dot + 5))
		} else {
			for decs < 4 {
				s = (s + "0")
				decs = (decs + 1)
			}
		}
	}
	return s
}

func padInt(n int, width int) string {
	var s string = fmt.Sprint(n)
	_ = s
	for len(s) < width {
		s = (" " + s)
	}
	return s
}

func padFloat(x float64, width int) string {
	var s string = fmtF(x)
	_ = s
	for len(s) < width {
		s = (" " + s)
	}
	return s
}

func avgLen(n int) float64 {
	var tests int = 10000
	_ = tests
	var sum int = 0
	_ = sum
	var seed int = 1
	_ = seed
	var t int = 0
	_ = t
	for t < tests {
		var visited []bool = []bool{}
		_ = visited
		var i int = 0
		_ = i
		for i < n {
			visited = append(visited, false)
			i = (i + 1)
		}
		var x int = 0
		_ = x
		for !visited[x] {
			visited[x] = true
			sum = (sum + 1)
			seed = (((seed * 1664525) + 1013904223) % 2147483647)
			x = (seed % n)
		}
		t = (t + 1)
	}
	return (float64(sum) / float64(tests))
}

func ana(n int) float64 {
	var nn float64 = float64(n)
	_ = nn
	var term float64 = 1.0
	_ = term
	var sum float64 = 1.0
	_ = sum
	var i float64 = (nn - 1.0)
	_ = i
	for i >= 1.0 {
		term = (term * (i / nn))
		sum = (sum + term)
		i = (i - 1.0)
	}
	return sum
}

func mochiMain() {
	var nmax int = 20
	_ = nmax
	fmt.Println(" N    average    analytical    (error)")
	fmt.Println("===  =========  ============  =========")
	var n int = 1
	_ = n
	for n <= nmax {
		var a float64 = avgLen(n)
		_ = a
		var b float64 = ana(n)
		_ = b
		var err float64 = ((absf((a - b)) / b) * 100.0)
		_ = err
		var line string = (((((((padInt(n, 3) + "  ") + padFloat(a, 9)) + "  ") + padFloat(b, 12)) + "  (") + padFloat(err, 6)) + "%)")
		_ = line
		fmt.Println(line)
		n = (n + 1)
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
