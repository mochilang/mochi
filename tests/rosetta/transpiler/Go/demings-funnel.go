//go:build ignore

// Generated by Mochi v0.10.42 on 2025-07-27 17:10:31 GMT+7
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

func sqrtApprox(x float64) float64 {
	if x <= 0.0 {
		return 0.0
	}
	var g float64 = x
	var i int = 0
	for i < 20 {
		g = ((g + (x / g)) / 2.0)
		i = (i + 1)
	}
	return g
}

var dxs []float64

var dys []float64

func funnel(fa []float64, r func(float64, float64) float64) []float64 {
	var x float64 = 0.0
	var result []any = []any{}
	var i int = 0
	for i < len(fa) {
		var f float64 = fa[i]
		result = append(result, (x + f))
		x = r(x, f)
		i = (i + 1)
	}
	return func(v any) []float64 {
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
	}(result)
}

func mean(fa []float64) float64 {
	var sum float64 = 0.0
	var i int = 0
	for i < len(fa) {
		sum = (sum + fa[i])
		i = (i + 1)
	}
	return (sum / float64(len(fa)))
}

func stdDev(fa []float64) float64 {
	var m float64 = mean(func(v any) []float64 {
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
	}(fa))
	var sum float64 = 0.0
	var i int = 0
	for i < len(fa) {
		var d float64 = (fa[i] - m)
		sum = (sum + (d * d))
		i = (i + 1)
	}
	var r float64 = sqrtApprox((sum / float64(len(fa))))
	return r
}

func experiment(label string, r func(float64, float64) float64) {
	var rxs []float64 = funnel(func(v any) []float64 {
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
	}(dxs), r)
	var rys []float64 = funnel(func(v any) []float64 {
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
	}(dys), r)
	fmt.Println((label + "  :      x        y"))
	fmt.Println(((("Mean    :  " + fmt.Sprint(mean(func(v any) []float64 {
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
	}(rxs)))) + ", ") + fmt.Sprint(mean(func(v any) []float64 {
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
	}(rys)))))
	fmt.Println(((("Std Dev :  " + fmt.Sprint(stdDev(func(v any) []float64 {
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
	}(rxs)))) + ", ") + fmt.Sprint(stdDev(func(v any) []float64 {
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
	}(rys)))))
	fmt.Println("")
}

func mochiMain() {
	experiment("Rule 1", func(x float64, y float64) float64 {
		return 0.0
	})
	experiment("Rule 2", func(x float64, dz float64) float64 {
		return (0 - dz)
	})
	experiment("Rule 3", func(z float64, dz float64) float64 {
		return (0 - (z + dz))
	})
	experiment("Rule 4", func(z float64, dz float64) float64 {
		return (z + dz)
	})
}

func main() {
	func() {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem := ms.Alloc
		benchStart := time.Now().UnixNano()
		dxs = []float64{(0 - 0.533), 0.27, 0.859, (0 - 0.043), (0 - 0.205), (0 - 0.127), (0 - 0.071), 0.275, 1.251, (0 - 0.231), (0 - 0.401), 0.269, 0.491, 0.951, 1.15, 0.001, (0 - 0.382), 0.161, 0.915, 2.08, (0 - 2.337), 0.034, (0 - 0.126), 0.014, 0.709, 0.129, (0 - 1.093), (0 - 0.483), (0 - 1.193), 0.02, (0 - 0.051), 0.047, (0 - 0.095), 0.695, 0.34, (0 - 0.182), 0.287, 0.213, (0 - 0.423), (0 - 0.021), (0 - 0.134), 1.798, 0.021, (0 - 1.099), (0 - 0.361), 1.636, (0 - 1.134), 1.315, 0.201, 0.034, 0.097, (0 - 0.17), 0.054, (0 - 0.553), (0 - 0.024), (0 - 0.181), (0 - 0.7), (0 - 0.361), (0 - 0.789), 0.279, (0 - 0.174), (0 - 0.009), (0 - 0.323), (0 - 0.658), 0.348, (0 - 0.528), 0.881, 0.021, (0 - 0.853), 0.157, 0.648, 1.774, (0 - 1.043), 0.051, 0.021, 0.247, (0 - 0.31), 0.171, 0.0, 0.106, 0.024, (0 - 0.386), 0.962, 0.765, (0 - 0.125), (0 - 0.289), 0.521, 0.017, 0.281, (0 - 0.749), (0 - 0.149), (0 - 2.436), (0 - 0.909), 0.394, (0 - 0.113), (0 - 0.598), 0.443, (0 - 0.521), (0 - 0.799), 0.087}
		dys = []float64{0.136, 0.717, 0.459, (0 - 0.225), 1.392, 0.385, 0.121, (0 - 0.395), 0.49, (0 - 0.682), (0 - 0.065), 0.242, (0 - 0.288), 0.658, 0.459, 0.0, 0.426, 0.205, (0 - 0.765), (0 - 2.188), (0 - 0.742), (0 - 0.01), 0.089, 0.208, 0.585, 0.633, (0 - 0.444), (0 - 0.351), (0 - 1.087), 0.199, 0.701, 0.096, (0 - 0.025), (0 - 0.868), 1.051, 0.157, 0.216, 0.162, 0.249, (0 - 0.007), 0.009, 0.508, (0 - 0.79), 0.723, 0.881, (0 - 0.508), 0.393, (0 - 0.226), 0.71, 0.038, (0 - 0.217), 0.831, 0.48, 0.407, 0.447, (0 - 0.295), 1.126, 0.38, 0.549, (0 - 0.445), (0 - 0.046), 0.428, (0 - 0.074), 0.217, (0 - 0.822), 0.491, 1.347, (0 - 0.141), 1.23, (0 - 0.044), 0.079, 0.219, 0.698, 0.275, 0.056, 0.031, 0.421, 0.064, 0.721, 0.104, (0 - 0.729), 0.65, (0 - 1.103), 0.154, (0 - 1.72), 0.051, (0 - 0.385), 0.477, 1.537, (0 - 0.901), 0.939, (0 - 0.411), 0.341, (0 - 0.411), 0.106, 0.224, (0 - 0.947), (0 - 1.424), (0 - 0.542), (0 - 1.032)}
		mochiMain()
		runtime.ReadMemStats(&ms)
		endMem := ms.Alloc
		benchEnd := time.Now().UnixNano()
		data := map[string]any{"name": "main", "duration_us": (benchEnd - benchStart) / 1000, "memory_bytes": endMem - startMem}
		out, _ := json.MarshalIndent(data, "", "  ")
		fmt.Println(string(out))
	}()
}
