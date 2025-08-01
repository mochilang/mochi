//go:build ignore

// Generated by Mochi v0.10.42 on 2025-07-27 23:54:10 GMT+7
package main

import (
	"encoding/json"
	"fmt"
	"os"
	"runtime"
	"strconv"
	"strings"
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

func floorf(x float64) float64 {
	var y int = int(x)
	_ = y
	_ = y
	return float64(y)
}

func powf(base float64, exp int) float64 {
	var r float64 = 1.0
	_ = r
	var i int = 0
	_ = i
	for i < exp {
		r = (r * base)
		i = (i + 1)
	}
	return r
}

func fmtF(x float64, width int, prec int) string {
	var factor float64 = powf(10.0, prec)
	_ = factor
	var y float64 = (floorf(((x * factor) + 0.5)) / factor)
	_ = y
	var s string = fmt.Sprint(y)
	_ = s
	var dot int = strings.Index(s, ".")
	_ = dot
	if dot == (0 - 1) {
		s = (s + ".")
		var j int = 0
		_ = j
		for j < prec {
			s = (s + "0")
			j = (j + 1)
		}
	} else {
		var decs int = ((len(s) - dot) - 1)
		_ = decs
		for decs < prec {
			s = (s + "0")
			decs = (decs + 1)
		}
	}
	for len(s) < width {
		s = (" " + s)
	}
	return s
}

func expf(x float64) float64 {
	if x < 0.0 {
		return (1.0 / expf((0 - x)))
	}
	var term float64 = 1.0
	_ = term
	var sum float64 = 1.0
	_ = sum
	var i int = 1
	_ = i
	for i < 20 {
		term = ((term * x) / float64(i))
		sum = (sum + term)
		i = (i + 1)
	}
	return sum
}

func eulerStep(f func(float64, float64) float64, x float64, y float64, h float64) float64 {
	return (y + (h * f(x, y)))
}

func newCoolingRate(k float64) func(float64) float64 {
	return func(dt float64) float64 {
		return ((0 - k) * dt)
	}
}

func newTempFunc(k float64, ambient float64, initial float64) func(float64) float64 {
	return func(t float64) float64 {
		return (ambient + ((initial - ambient) * expf(((0 - k) * t))))
	}
}

func newCoolingRateDy(k float64, ambient float64) func(float64, float64) float64 {
	cr := newCoolingRate(k)
	_ = cr
	_ = cr
	return func(_x float64, obj float64) float64 {
		return cr((obj - ambient))
	}
}

func mochiMain() {
	var k float64 = 0.07
	_ = k
	var tempRoom float64 = 20.0
	_ = tempRoom
	var tempObject float64 = 100.0
	_ = tempObject
	fcr := newCoolingRateDy(k, tempRoom)
	_ = fcr
	analytic := newTempFunc(k, tempRoom, tempObject)
	_ = analytic
	_ = analytic
	for _, step := range []float64{2.0, 5.0, 10.0} {
		fmt.Println(("Step size = " + fmtF(step, 0, 1)))
		fmt.Println(" Time Euler's Analytic")
		var temp float64 = tempObject
		_ = temp
		var time float64 = 0.0
		_ = time
		for time <= 100.0 {
			var line string = ((((fmtF(time, 5, 1) + " ") + fmtF(temp, 7, 3)) + " ") + fmtF(analytic(time), 7, 3))
			_ = line
			fmt.Println(line)
			temp = eulerStep(fcr, time, temp, step)
			time = (time + step)
		}
		fmt.Println("")
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
