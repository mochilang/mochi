//go:build ignore

// Generated by Mochi v0.10.40 on 2025-07-26 16:54:21 GMT+7
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

var width int

var height int

var iterations int

var grid [][]string

var y int

func randInt(s int, n int) []int {
	var next int = (((s * 1664525) + 1013904223) % 2147483647)
	_ = next
	return []int{next, (next % n)}
}

var seed int

var vertices [][]int

var px int

var py int

var i int

func main() {
	func() {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem := ms.Alloc
		benchStart := time.Now().UnixNano()
		width = 60
		height = int((float64(width) * 0.86602540378))
		iterations = 5000
		grid = [][]string{}
		y = 0
		for y < height {
			var line []string = []string{}
			_ = line
			var x int = 0
			_ = x
			for x < width {
				line = append(line, " ")
				x = (x + 1)
			}
			grid = append(grid, line)
			y = (y + 1)
		}
		seed = 1
		vertices = [][]int{[]int{0, (height - 1)}, []int{(width - 1), (height - 1)}, []int{int((width / 2)), 0}}
		px = int((width / 2))
		py = int((height / 2))
		i = 0
		for i < iterations {
			var r []int = randInt(seed, 3)
			_ = r
			seed = r[0]
			var idx int = int(r[1])
			_ = idx
			var v []int = vertices[idx]
			_ = v
			px = int(((px + v[0]) / 2))
			py = int(((py + v[1]) / 2))
			if (((px >= 0) && (px < width)) && (py >= 0)) && (py < height) {
				grid[py][px] = "*"
			}
			i = (i + 1)
		}
		y = 0
		for y < height {
			var line string = ""
			_ = line
			var x int = 0
			_ = x
			for x < width {
				line = (line + grid[y][x])
				x = (x + 1)
			}
			fmt.Println(line)
			y = (y + 1)
		}
		runtime.ReadMemStats(&ms)
		endMem := ms.Alloc
		benchEnd := time.Now().UnixNano()
		data := map[string]any{"name": "main", "duration_us": (benchEnd - benchStart) / 1000, "memory_bytes": endMem - startMem}
		out, _ := json.MarshalIndent(data, "", "  ")
		fmt.Println(string(out))
	}()
}
