//go:build ignore

// Generated by Mochi v0.10.42 on 2025-07-27 21:52:47 GMT+7
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

type DotResult struct {
	Value int  `json:"value"`
	Ok    bool `json:"ok"`
}

func dot(x []int, y []int) DotResult {
	if len(x) != len(y) {
		return DotResult{
			Value: 0,
			Ok:    false,
		}
	}
	var sum int = 0
	var i int = 0
	for i < len(x) {
		sum = (sum + (x[i] * y[i]))
		i = (i + 1)
	}
	return DotResult{
		Value: sum,
		Ok:    true,
	}
}

func mochiMain() {
	var r DotResult = dot(func(v any) []int {
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
	}([]int{1, 3, (0 - 5)}), func(v any) []int {
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
	}([]int{4, (0 - 2), (0 - 1)}))
	if !r.Ok {
		fmt.Println("incompatible lengths")
	} else {
		fmt.Println(fmt.Sprint(r.Value))
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
