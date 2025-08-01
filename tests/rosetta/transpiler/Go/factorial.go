//go:build ignore

// Generated by Mochi v0.10.55 on 2025-08-02 17:45:35 GMT+7
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

func factorial(n int) *big.Int {
	var r *big.Int = big.NewInt(int64(1))
	_ = r
	var i int = 2
	_ = i
	for i <= n {
		r = new(big.Int).Mul(r, big.NewInt(int64(i)))
		i = (i + 1)
	}
	return r
}

func main() {
	func() {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem := ms.Alloc
		benchStart := time.Now().UnixNano()
		for i := 0; i < 11; i++ {
			fmt.Println(((fmt.Sprint(i) + " ") + fmt.Sprint(factorial(i))))
		}
		fmt.Println(("100 " + fmt.Sprint(factorial(100))))
		fmt.Println(("800 " + fmt.Sprint(factorial(800))))
		runtime.ReadMemStats(&ms)
		endMem := ms.Alloc
		benchEnd := time.Now().UnixNano()
		data := map[string]any{"name": "main", "duration_us": (benchEnd - benchStart) / 1000, "memory_bytes": endMem - startMem}
		out, _ := json.MarshalIndent(data, "", "  ")
		fmt.Println(string(out))
	}()
}
