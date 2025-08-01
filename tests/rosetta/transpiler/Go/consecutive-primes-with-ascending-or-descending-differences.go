//go:build ignore

// Generated by Mochi v0.10.54 on 2025-08-02 14:40:28 GMT+7
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

func primesUpTo(n int) []int {
	var sieve []bool = []bool{}
	_ = sieve
	var i int = 0
	_ = i
	for i <= n {
		sieve = append(sieve, true)
		i = (i + 1)
	}
	var p int = 2
	_ = p
	for (p * p) <= n {
		if sieve[p] {
			var m int = (p * p)
			_ = m
			for m <= n {
				sieve[m] = false
				m = (m + p)
			}
		}
		p = (p + 1)
	}
	var res []int = []int{}
	_ = res
	var x int = 2
	_ = x
	for x <= n {
		if sieve[x] {
			res = append(res, x)
		}
		x = (x + 1)
	}
	return res
}

var LIMIT int

var primes []int

func longestSeq(dir string) {
	var pd int = 0
	_ = pd
	var longSeqs [][]int = [][]int{[]int{2}}
	_ = longSeqs
	var currSeq []int = []int{2}
	_ = currSeq
	var i int = 1
	_ = i
	for i < len(primes) {
		var d int = (primes[i] - primes[(i-1)])
		_ = d
		if ((dir == "ascending") && (d <= pd)) || ((dir == "descending") && (d >= pd)) {
			if len(currSeq) > len(longSeqs[0]) {
				longSeqs = [][]int{currSeq}
			} else {
				if len(currSeq) == len(longSeqs[0]) {
					longSeqs = append(longSeqs, currSeq)
				}
			}
			currSeq = []int{primes[(i - 1)], primes[i]}
		} else {
			currSeq = append(currSeq, primes[i])
		}
		pd = d
		i = (i + 1)
	}
	if len(currSeq) > len(longSeqs[0]) {
		longSeqs = [][]int{currSeq}
	} else {
		if len(currSeq) == len(longSeqs[0]) {
			longSeqs = append(longSeqs, currSeq)
		}
	}
	fmt.Println((((("Longest run(s) of primes with " + dir) + " differences is ") + fmt.Sprint(len(longSeqs[0]))) + " :"))
	for _, ls := range longSeqs {
		var diffs []int = []int{}
		_ = diffs
		var j int = 1
		_ = j
		for j < len(ls) {
			diffs = append(diffs, (ls[j] - ls[(j-1)]))
			j = (j + 1)
		}
		var k int = 0
		_ = k
		for k < (len(ls) - 1) {
			fmt.Println((((fmt.Sprint(ls[k]) + " (") + fmt.Sprint(diffs[k])) + ") "), false)
			k = (k + 1)
		}
		fmt.Println(fmt.Sprint(ls[(len(ls) - 1)]))
	}
	fmt.Println("")
}

func mochiMain() {
	fmt.Println("For primes < 1 million:\n")
	for _, dir := range []string{"ascending", "descending"} {
		longestSeq(dir)
	}
}

func main() {
	func() {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem := ms.Alloc
		benchStart := time.Now().UnixNano()
		LIMIT = 999999
		primes = primesUpTo(LIMIT)
		mochiMain()
		runtime.ReadMemStats(&ms)
		endMem := ms.Alloc
		benchEnd := time.Now().UnixNano()
		data := map[string]any{"name": "main", "duration_us": (benchEnd - benchStart) / 1000, "memory_bytes": endMem - startMem}
		out, _ := json.MarshalIndent(data, "", "  ")
		fmt.Println(string(out))
	}()
}
