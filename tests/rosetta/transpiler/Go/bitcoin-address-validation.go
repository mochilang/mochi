//go:build ignore

// Generated by Mochi v0.10.40 on 2025-07-25 21:14:39 GMT+7
package main

import (
	"crypto/sha256"
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

func _sha256(bs []int) []int {
	b := make([]byte, len(bs))
	for i, v := range bs {
		b[i] = byte(v)
	}
	h := sha256.Sum256(b)
	out := make([]int, len(h))
	for i, v := range h[:] {
		out[i] = int(v)
	}
	return out
}

func indexOf(s string, ch string) int {
	var i int = 0
	_ = i
	for i < len(s) {
		if string([]rune(s)[i:(i+1)]) == ch {
			return i
		}
		i = (i + 1)
	}
	return (0 - 1)
}

func set58(addr string) []int {
	var tmpl string = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
	_ = tmpl
	var a []int = []int{}
	_ = a
	var i int = 0
	_ = i
	for i < 25 {
		a = append(a, 0)
		i = (i + 1)
	}
	var idx int = 0
	_ = idx
	for idx < len(addr) {
		var ch string = string([]rune(addr)[idx:(idx + 1)])
		_ = ch
		var c int = indexOf(tmpl, ch)
		_ = c
		if c < 0 {
			return []int{}
		}
		var j int = 24
		_ = j
		for j >= 0 {
			c = (c + (58 * a[j]))
			a[j] = (c % 256)
			c = int((c / 256))
			j = (j - 1)
		}
		if c > 0 {
			return []int{}
		}
		idx = (idx + 1)
	}
	return a
}

func doubleSHA256(bs []int) []int {
	var first []int = _sha256(bs)
	_ = first
	return _sha256(first)
}

func computeChecksum(a []int) []int {
	var hash []int = doubleSHA256(a[0:21])
	_ = hash
	return hash[0:4]
}

func validA58(addr string) bool {
	var a []int = set58(addr)
	_ = a
	if len(a) != 25 {
		return false
	}
	if a[0] != 0 {
		return false
	}
	var sum []int = computeChecksum(a)
	_ = sum
	var i int = 0
	_ = i
	for i < 4 {
		if a[(21+i)] != sum[i] {
			return false
		}
		i = (i + 1)
	}
	return true
}

func main() {
	func() {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem := ms.Alloc
		start := _now()
		fmt.Println(fmt.Sprint(validA58("1AGNa15ZQXAZUgFiqJ3i7Z2DPU2J6hW62i")))
		fmt.Println(fmt.Sprint(validA58("17NdbrSGoUotzeGCcMMCqnFkEvLymoou9j")))
		runtime.ReadMemStats(&ms)
		endMem := ms.Alloc
		end := _now()
		data := map[string]any{"name": "main", "duration_us": (end - start) / 1000, "memory_bytes": endMem - startMem}
		out, _ := json.MarshalIndent(data, "", "  ")
		fmt.Println(string(out))
	}()
}
