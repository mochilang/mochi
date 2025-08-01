//go:build ignore

// Generated by Mochi v0.10.52 on 2025-08-02 01:03:26 GMT+7
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

func sel(list []float64, k int) float64 {
	var i int = 0
	_ = i
	for i <= k {
		var minIndex int = i
		_ = minIndex
		var j int = (i + 1)
		_ = j
		for j < len(list) {
			if list[j] < list[minIndex] {
				minIndex = j
			}
			j = (j + 1)
		}
		var tmp float64 = list[i]
		_ = tmp
		list[i] = list[minIndex]
		list[minIndex] = tmp
		i = (i + 1)
	}
	return list[k]
}

func median(a []float64) float64 {
	var arr []float64 = a
	_ = arr
	var half int = int((len(arr) / 2))
	_ = half
	var med float64 = sel(func(v any) []float64 {
		if v == nil {
			return nil
		}
		if vv, ok := v.([]float64); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return []float64{}
			}
			out := make([]float64, len(arr))
			for i, x := range arr {
				out[i] = x.(float64)
			}
			return out
		}
		return v.([]float64)
	}(arr), half)
	_ = med
	_ = med
	if (len(arr) % 2) == 0 {
		return ((med + arr[(half-1)]) / 2.0)
	}
	return med
}

func main() {
	func() {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem := ms.Alloc
		benchStart := time.Now().UnixNano()
		fmt.Println(fmt.Sprint(median(func(v any) []float64 {
			if v == nil {
				return nil
			}
			if vv, ok := v.([]float64); ok {
				return vv
			}
			if arr, ok := v.([]any); ok {
				if len(arr) == 0 {
					return []float64{}
				}
				out := make([]float64, len(arr))
				for i, x := range arr {
					out[i] = x.(float64)
				}
				return out
			}
			return v.([]float64)
		}([]float64{3.0, 1.0, 4.0, 1.0}))))
		fmt.Println(fmt.Sprint(median(func(v any) []float64 {
			if v == nil {
				return nil
			}
			if vv, ok := v.([]float64); ok {
				return vv
			}
			if arr, ok := v.([]any); ok {
				if len(arr) == 0 {
					return []float64{}
				}
				out := make([]float64, len(arr))
				for i, x := range arr {
					out[i] = x.(float64)
				}
				return out
			}
			return v.([]float64)
		}([]float64{3.0, 1.0, 4.0, 1.0, 5.0}))))
		runtime.ReadMemStats(&ms)
		endMem := ms.Alloc
		benchEnd := time.Now().UnixNano()
		data := map[string]any{"name": "main", "duration_us": (benchEnd - benchStart) / 1000, "memory_bytes": endMem - startMem}
		out, _ := json.MarshalIndent(data, "", "  ")
		fmt.Println(string(out))
	}()
}
