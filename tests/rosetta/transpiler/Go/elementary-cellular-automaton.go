//go:build ignore

// Generated by Mochi v0.10.42 on 2025-07-27 22:56:17 GMT+7
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

func bitAt(x int, idx int) int {
	var v int = x
	_ = v
	var i int = 0
	_ = i
	for i < idx {
		v = int((v / 2))
		i = (i + 1)
	}
	return (v % 2)
}

func outputState(state string) {
	var line string = ""
	_ = line
	var i int = 0
	_ = i
	for i < len(state) {
		if string([]rune(state)[i:(i+1)]) == "1" {
			line = (line + "#")
		} else {
			line = (line + " ")
		}
		i = (i + 1)
	}
	fmt.Println(line)
}

func step(state string, r int) string {
	var cells int = len(state)
	_ = cells
	var out string = ""
	_ = out
	var i int = 0
	_ = i
	for i < cells {
		var l string = string([]rune(state)[(((i - 1) + cells) % cells):((((i - 1) + cells) % cells) + 1)])
		_ = l
		var c string = string([]rune(state)[i:(i + 1)])
		_ = c
		var rt string = string([]rune(state)[((i + 1) % cells):(((i + 1) % cells) + 1)])
		_ = rt
		var idx int = 0
		_ = idx
		if l == "1" {
			idx = (idx + 4)
		}
		if c == "1" {
			idx = (idx + 2)
		}
		if rt == "1" {
			idx = (idx + 1)
		}
		if bitAt(r, idx) == 1 {
			out = (out + "1")
		} else {
			out = (out + "0")
		}
		i = (i + 1)
	}
	return out
}

func elem(r int, cells int, generations int, state string) {
	outputState(state)
	var g int = 0
	_ = g
	var s string = state
	_ = s
	for g < generations {
		s = step(s, r)
		outputState(s)
		g = (g + 1)
	}
}

func randInit(cells int, seed int) string {
	var s string = ""
	_ = s
	var val int = seed
	_ = val
	var i int = 0
	_ = i
	for i < cells {
		val = (((val * 1664525) + 1013904223) % 2147483647)
		if (val % 2) == 0 {
			s = (s + "0")
		} else {
			s = (s + "1")
		}
		i = (i + 1)
	}
	return s
}

func singleInit(cells int) string {
	var s string = ""
	_ = s
	var i int = 0
	_ = i
	for i < cells {
		if i == (cells / 2) {
			s = (s + "1")
		} else {
			s = (s + "0")
		}
		i = (i + 1)
	}
	return s
}

func mochiMain() {
	var cells int = 20
	_ = cells
	var generations int = 9
	_ = generations
	fmt.Println("Single 1, rule 90:")
	var state string = singleInit(cells)
	_ = state
	elem(90, cells, generations, state)
	fmt.Println("Random intial state, rule 30:")
	state = randInit(cells, 3)
	elem(30, cells, generations, state)
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
