//go:build ignore

// Generated by Mochi v0.10.52 on 2025-08-02 01:04:06 GMT+7
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

func absi(x int) int {
	if x < 0 {
		return (0 - x)
	}
	return x
}

type Point struct {
	X int `json:"x"`
	Y int `json:"y"`
}

func bresenham(x0 int, y0 int, x1 int, y1 int) []Point {
	var dx int = absi((x1 - x0))
	_ = dx
	var dy int = absi((y1 - y0))
	_ = dy
	var sx int = (0 - 1)
	_ = sx
	if x0 < x1 {
		sx = 1
	}
	var sy int = (0 - 1)
	_ = sy
	if y0 < y1 {
		sy = 1
	}
	var err int = (dx - dy)
	_ = err
	var pts []Point = []Point{}
	_ = pts
	for {
		pts = append(pts, Point{
			X: x0,
			Y: y0,
		})
		if (x0 == x1) && (y0 == y1) {
			break
		}
		var e2 int = (2 * err)
		_ = e2
		if e2 > (0 - dy) {
			err = (err - dy)
			x0 = (x0 + sx)
		}
		if e2 < dx {
			err = (err + dx)
			y0 = (y0 + sy)
		}
	}
	return pts
}

func mochiMain() {
	var pts []Point = bresenham(0, 0, 6, 4)
	_ = pts
	var i int = 0
	_ = i
	for i < len(pts) {
		var p Point = pts[i]
		_ = p
		fmt.Println((((("(" + fmt.Sprint(p.X)) + ",") + fmt.Sprint(p.Y)) + ")"))
		i = (i + 1)
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
