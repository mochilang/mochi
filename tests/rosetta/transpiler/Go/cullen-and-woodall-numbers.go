//go:build ignore

// Generated by Mochi v0.10.42 on 2025-07-27 16:25:53 GMT+7
package main

import (
	"encoding/json"
	"fmt"
	"math/big"
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

func pow_big(base *big.Int, exp int) *big.Int {
	var result *big.Int = big.NewInt(int64(1))
	var b *big.Int = base
	var e int = exp
	for e > 0 {
		if (e % 2) == 1 {
			result = new(big.Int).Mul(result, b)
		}
		b = new(big.Int).Mul(b, b)
		e = int((e / 2))
	}
	return result
}

func cullen(n int) *big.Int {
	var two_n *big.Int = pow_big(big.NewInt(int64(2)), n)
	return new(big.Int).Add(new(big.Int).Mul(two_n, big.NewInt(int64(n))), big.NewInt(int64(1)))
}

func woodall(n int) *big.Int {
	return new(big.Int).Sub(cullen(n), big.NewInt(int64(2)))
}

func show_list(xs []*big.Int) string {
	var line string = ""
	var i int = 0
	for i < len(xs) {
		line = (line + fmt.Sprint(xs[i]))
		if i < (len(xs) - 1) {
			line = (line + " ")
		}
		i = (i + 1)
	}
	return line
}

func mochiMain() {
	var cnums []*big.Int = []*big.Int{}
	var i int = 1
	for i <= 20 {
		cnums = append(cnums, cullen(i))
		i = (i + 1)
	}
	fmt.Println("First 20 Cullen numbers (n * 2^n + 1):")
	fmt.Println(show_list(func(v any) []*big.Int {
		if v == nil {
			return nil
		}
		if vv, ok := v.([]*big.Int); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return []*big.Int{}
			}
			out := make([]*big.Int, len(arr))
			for i, x := range arr {
				out[i] = x.(*big.Int)
			}
			return out
		}
		return v.([]*big.Int)
	}(cnums)))
	var wnums []*big.Int = []*big.Int{}
	i = 1
	for i <= 20 {
		wnums = append(wnums, woodall(i))
		i = (i + 1)
	}
	fmt.Println("\nFirst 20 Woodall numbers (n * 2^n - 1):")
	fmt.Println(show_list(func(v any) []*big.Int {
		if v == nil {
			return nil
		}
		if vv, ok := v.([]*big.Int); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return []*big.Int{}
			}
			out := make([]*big.Int, len(arr))
			for i, x := range arr {
				out[i] = x.(*big.Int)
			}
			return out
		}
		return v.([]*big.Int)
	}(wnums)))
	var cprimes []*big.Int = []*big.Int{big.NewInt(int64(1)), big.NewInt(int64(141)), big.NewInt(int64(4713)), big.NewInt(int64(5795)), big.NewInt(int64(6611))}
	fmt.Println("\nFirst 5 Cullen primes (in terms of n):")
	fmt.Println(show_list(func(v any) []*big.Int {
		if v == nil {
			return nil
		}
		if vv, ok := v.([]*big.Int); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return []*big.Int{}
			}
			out := make([]*big.Int, len(arr))
			for i, x := range arr {
				out[i] = x.(*big.Int)
			}
			return out
		}
		return v.([]*big.Int)
	}(cprimes)))
	var wprimes []*big.Int = []*big.Int{big.NewInt(int64(2)), big.NewInt(int64(3)), big.NewInt(int64(6)), big.NewInt(int64(30)), big.NewInt(int64(75)), big.NewInt(int64(81)), big.NewInt(int64(115)), big.NewInt(int64(123)), big.NewInt(int64(249)), big.NewInt(int64(362)), big.NewInt(int64(384)), big.NewInt(int64(462))}
	fmt.Println("\nFirst 12 Woodall primes (in terms of n):")
	fmt.Println(show_list(func(v any) []*big.Int {
		if v == nil {
			return nil
		}
		if vv, ok := v.([]*big.Int); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return []*big.Int{}
			}
			out := make([]*big.Int, len(arr))
			for i, x := range arr {
				out[i] = x.(*big.Int)
			}
			return out
		}
		return v.([]*big.Int)
	}(wprimes)))
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
