//go:build ignore

// Generated by Mochi v0.10.42 on 2025-07-27 17:10:52 GMT+7
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

func powInt(base int, exp int) int {
	var r int = 1
	var b int = base
	var e int = exp
	for e > 0 {
		if (e % 2) == 1 {
			r = (r * b)
		}
		b = (b * b)
		e = (e / int(2))
	}
	return r
}

func minInt(x int, y int) int {
	if x < y {
		return x
	}
	return y
}

func throwDie(nSides int, nDice int, s int, counts []int) {
	if nDice == 0 {
		counts[s] = (counts[s] + 1)
		return
	}
	var i int = 1
	for i <= nSides {
		throwDie(nSides, (nDice - 1), (s + i), func(v any) []int {
			if v == nil {
				return nil
			}
			if vv, ok := v.([]int); ok {
				return vv
			}
			if arr, ok := v.([]any); ok {
				if len(arr) == 0 {
					return []int{}
				}
				out := make([]int, len(arr))
				for i, x := range arr {
					out[i] = x.(int)
				}
				return out
			}
			return v.([]int)
		}(counts))
		i = (i + 1)
	}
}

func beatingProbability(nSides1 int, nDice1 int, nSides2 int, nDice2 int) float64 {
	var len1 int = ((nSides1 + 1) * nDice1)
	var c1 []int = []int{}
	var i int = 0
	for i < len1 {
		c1 = append(c1, 0)
		i = (i + 1)
	}
	throwDie(nSides1, nDice1, 0, func(v any) []int {
		if v == nil {
			return nil
		}
		if vv, ok := v.([]int); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return []int{}
			}
			out := make([]int, len(arr))
			for i, x := range arr {
				out[i] = x.(int)
			}
			return out
		}
		return v.([]int)
	}(c1))
	var len2 int = ((nSides2 + 1) * nDice2)
	var c2 []int = []int{}
	var j int = 0
	for j < len2 {
		c2 = append(c2, 0)
		j = (j + 1)
	}
	throwDie(nSides2, nDice2, 0, func(v any) []int {
		if v == nil {
			return nil
		}
		if vv, ok := v.([]int); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return []int{}
			}
			out := make([]int, len(arr))
			for i, x := range arr {
				out[i] = x.(int)
			}
			return out
		}
		return v.([]int)
	}(c2))
	var p12 float64 = (float64(powInt(nSides1, nDice1)) * float64(powInt(nSides2, nDice2)))
	var tot float64 = 0.0
	i = 0
	for i < len1 {
		j = 0
		var m int = minInt(i, len2)
		for j < m {
			tot = (tot + ((float64(c1[i]) * float64(c2[j])) / p12))
			j = (j + 1)
		}
		i = (i + 1)
	}
	return tot
}

func main() {
	func() {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem := ms.Alloc
		benchStart := time.Now().UnixNano()
		fmt.Println(fmt.Sprint(beatingProbability(4, 9, 6, 6)))
		fmt.Println(fmt.Sprint(beatingProbability(10, 5, 7, 6)))
		runtime.ReadMemStats(&ms)
		endMem := ms.Alloc
		benchEnd := time.Now().UnixNano()
		data := map[string]any{"name": "main", "duration_us": (benchEnd - benchStart) / 1000, "memory_bytes": endMem - startMem}
		out, _ := json.MarshalIndent(data, "", "  ")
		fmt.Println(string(out))
	}()
}
