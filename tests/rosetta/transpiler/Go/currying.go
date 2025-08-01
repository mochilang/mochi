//go:build ignore

// Generated by Mochi v0.10.42 on 2025-07-27 16:25:58 GMT+7
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

func pow(base float64, exp float64) float64 {
	var result float64 = 1.0
	var i int = 0
	for i < int(exp) {
		result = (result * base)
		i = (i + 1)
	}
	return result
}

func PowN(b float64) func(float64) float64 {
	return func(e float64) float64 {
		return pow(b, e)
	}
}

func PowE(e float64) func(float64) float64 {
	return func(b float64) float64 {
		return pow(b, e)
	}
}

type Foo struct {
	Value int `json:"value"`
}

func (s *Foo) Method(b int) int {
	return (s.Value + b)
}

func mochiMain() {
	pow2 := PowN(2.0)
	cube := PowE(3.0)
	fmt.Println(("2^8 = " + fmt.Sprint(pow2(8.0))))
	fmt.Println(("4³ = " + fmt.Sprint(cube(4.0))))
	var a Foo = Foo{
		Value: 2,
	}
	fn1 := func(b int) int {
		return a.Method(b)
	}
	fn2 := func(f Foo, b int) int {
		return f.Method(b)
	}
	fmt.Println(("2 + 2 = " + fmt.Sprint(a.Method(2))))
	fmt.Println(("2 + 3 = " + fmt.Sprint(fn1(3))))
	fmt.Println(("2 + 4 = " + fmt.Sprint(fn2(a, 4))))
	fmt.Println(("3 + 5 = " + fmt.Sprint(fn2(Foo{
		Value: 3,
	}, 5))))
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
