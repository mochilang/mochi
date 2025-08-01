//go:build ignore

// Generated by Mochi v0.10.42 on 2025-07-27 22:51:14 GMT+7
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

type DivResult struct {
	Q int `json:"q"`
	R int `json:"r"`
}

func egyptianDivide(dividend int, divisor int) DivResult {
	if (dividend < 0) || (divisor <= 0) {
		panic("Invalid argument(s)")
	}
	if dividend < divisor {
		return DivResult{
			Q: 0,
			R: dividend,
		}
	}
	var powers []int = []int{1}
	_ = powers
	var doublings []int = []int{divisor}
	_ = doublings
	var doubling int = (divisor * 2)
	_ = doubling
	for doubling <= dividend {
		powers = append(powers, (powers[(len(powers)-1)] * 2))
		doublings = append(doublings, doubling)
		doubling = (doubling * 2)
	}
	var ans int = 0
	_ = ans
	var accum int = 0
	_ = accum
	var i int = (len(doublings) - 1)
	_ = i
	for i >= 0 {
		if (accum + doublings[i]) <= dividend {
			accum = (accum + doublings[i])
			ans = (ans + powers[i])
			if accum == dividend {
				break
			}
		}
		i = (i - 1)
	}
	return DivResult{
		Q: ans,
		R: (dividend - accum),
	}
}

func mochiMain() {
	var dividend int = 580
	_ = dividend
	var divisor int = 34
	_ = divisor
	var res DivResult = egyptianDivide(dividend, divisor)
	_ = res
	fmt.Println(((((((fmt.Sprint(dividend) + " divided by ") + fmt.Sprint(divisor)) + " is ") + fmt.Sprint(res.Q)) + " with remainder ") + fmt.Sprint(res.R)))
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
