//go:build ignore

// Generated by Mochi v0.10.55 on 2025-08-02 17:45:37 GMT+7
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

var qlimit int

func powf(base float64, exp int) float64 {
	var result float64 = 1.0
	_ = result
	var i int = 0
	_ = i
	for i < exp {
		result = (result * base)
		i = (i + 1)
	}
	return result
}

func sqrtApprox(x float64) float64 {
	if x <= 0.0 {
		return 0.0
	}
	var g float64 = x
	_ = g
	var i int = 0
	_ = i
	for i < 20 {
		g = ((g + (x / g)) / 2.0)
		i = (i + 1)
	}
	return g
}

func modPow(base int, exp int, mod int) int {
	var result int = (1 % mod)
	_ = result
	var b int = (base % mod)
	_ = b
	var e int = exp
	_ = e
	for e > 0 {
		if (e % 2) == 1 {
			result = ((result * b) % mod)
		}
		b = ((b * b) % mod)
		e = (e / 2)
	}
	return result
}

func mtest(m int) {
	if m < 4 {
		fmt.Println((((fmt.Sprint(m) + " < 4.  M") + fmt.Sprint(m)) + " not tested."))
		return
	}
	var flimit float64 = sqrtApprox((powf(2.0, m) - 1.0))
	_ = flimit
	var qlast int = 0
	_ = qlast
	if flimit < float64(qlimit) {
		qlast = int(flimit)
	} else {
		qlast = qlimit
	}
	var composite []bool = []bool{}
	_ = composite
	var i int = 0
	_ = i
	for i <= qlast {
		composite = append(composite, false)
		i = (i + 1)
	}
	var sq int = int(sqrtApprox(float64(qlast)))
	_ = sq
	var q int = 3
	_ = q
	for {
		if q <= sq {
			var j int = (q * q)
			_ = j
			for j <= qlast {
				composite[j] = true
				j = (j + q)
			}
		}
		var q8 int = (q % 8)
		_ = q8
		if ((q8 == 1) || (q8 == 7)) && (modPow(2, m, q) == 1) {
			fmt.Println(((("M" + fmt.Sprint(m)) + " has factor ") + fmt.Sprint(q)))
			return
		}
		for {
			q = (q + 2)
			if q > qlast {
				fmt.Println((("No factors of M" + fmt.Sprint(m)) + " found."))
				return
			}
			if !composite[q] {
				break
			}
		}
	}
}

func mochiMain() {
	mtest(31)
	mtest(67)
}

func main() {
	func() {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem := ms.Alloc
		benchStart := time.Now().UnixNano()
		qlimit = 50000
		mochiMain()
		runtime.ReadMemStats(&ms)
		endMem := ms.Alloc
		benchEnd := time.Now().UnixNano()
		data := map[string]any{"name": "main", "duration_us": (benchEnd - benchStart) / 1000, "memory_bytes": endMem - startMem}
		out, _ := json.MarshalIndent(data, "", "  ")
		fmt.Println(string(out))
	}()
}
