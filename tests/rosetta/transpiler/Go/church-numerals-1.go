//go:build ignore

// Generated by Mochi v0.10.41 on 2025-07-26 19:51:58 GMT+7
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

func zero(f func(any) any) func(any) any {
	return func(x any) any {
		return x
	}
}

func succ(c func(func(any) any) func(any) any) func(func(any) any) func(any) any {
	return func(f func(any) any) func(any) any {
		return func(x any) any {
			return f(c(f)(x))
		}
	}
}

func add(c func(func(any) any) func(any) any, d func(func(any) any) func(any) any) func(func(any) any) func(any) any {
	return func(f func(any) any) func(any) any {
		return func(x any) any {
			return c(f)(d(f)(x))
		}
	}
}

func mul(c func(func(any) any) func(any) any, d func(func(any) any) func(any) any) func(func(any) any) func(any) any {
	return func(f func(any) any) func(any) any {
		return func(x any) any {
			return c(d(f))(x)
		}
	}
}

func pow(c func(func(any) any) func(any) any, d func(func(any) any) func(any) any) func(func(any) any) func(any) any {
	var di int = toInt(d)
	_ = di
	prod := c
	_ = prod
	var i int = 1
	_ = i
	for i < di {
		prod = mul(prod, c)
		i = (i + 1)
	}
	return prod
}

func incr(i any) any {
	return (i.(int) + 1)
}

func toInt(c func(func(any) any) func(any) any) int {
	return c(incr)(0).(int)
}

func intToChurch(i int) func(func(any) any) func(any) any {
	if i == 0 {
		return zero
	}
	return succ(intToChurch((i - 1)))
}

var z func(func(any) any) func(any) any

var three func(func(any) any) func(any) any

var four func(func(any) any) func(any) any

func main() {
	func() {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem := ms.Alloc
		benchStart := time.Now().UnixNano()
		z = zero
		three = succ(succ(succ(z)))
		four = succ(three)
		fmt.Println(("three        -> " + fmt.Sprint(toInt(three))))
		fmt.Println(("four         -> " + fmt.Sprint(toInt(four))))
		fmt.Println(("three + four -> " + fmt.Sprint(toInt(add(three, four)))))
		fmt.Println(("three * four -> " + fmt.Sprint(toInt(mul(three, four)))))
		fmt.Println(("three ^ four -> " + fmt.Sprint(toInt(pow(three, four)))))
		fmt.Println(("four ^ three -> " + fmt.Sprint(toInt(pow(four, three)))))
		fmt.Println(("5 -> five    -> " + fmt.Sprint(toInt(intToChurch(5)))))
		runtime.ReadMemStats(&ms)
		endMem := ms.Alloc
		benchEnd := time.Now().UnixNano()
		data := map[string]any{"name": "main", "duration_us": (benchEnd - benchStart) / 1000, "memory_bytes": endMem - startMem}
		out, _ := json.MarshalIndent(data, "", "  ")
		fmt.Println(string(out))
	}()
}
