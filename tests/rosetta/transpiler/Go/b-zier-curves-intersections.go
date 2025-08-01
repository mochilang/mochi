//go:build ignore

// Generated by Mochi v0.10.52 on 2025-08-02 01:03:38 GMT+7
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

func absf(x float64) float64 {
	if x < 0.0 {
		return (0 - x)
	}
	return x
}

func maxf(a float64, b float64) float64 {
	if a > b {
		return a
	}
	return b
}

func minf(a float64, b float64) float64 {
	if a < b {
		return a
	}
	return b
}

func max3(a float64, b float64, c float64) float64 {
	var m float64 = a
	_ = m
	if b > m {
		m = b
	}
	if c > m {
		m = c
	}
	return m
}

func min3(a float64, b float64, c float64) float64 {
	var m float64 = a
	_ = m
	if b < m {
		m = b
	}
	if c < m {
		m = c
	}
	return m
}

type Point struct {
	X float64 `json:"x"`
	Y float64 `json:"y"`
}

type QuadSpline struct {
	C0 float64 `json:"c0"`
	C1 float64 `json:"c1"`
	C2 float64 `json:"c2"`
}

type QuadCurve struct {
	X QuadSpline `json:"x"`
	Y QuadSpline `json:"y"`
}

func subdivideQuadSpline(q QuadSpline, t float64) []QuadSpline {
	var s float64 = (1.0 - t)
	_ = s
	var u QuadSpline = QuadSpline{
		C0: q.C0,
		C1: 0.0,
		C2: 0.0,
	}
	_ = u
	var v QuadSpline = QuadSpline{
		C0: 0.0,
		C1: 0.0,
		C2: q.C2,
	}
	_ = v
	u.C1 = ((s * q.C0) + (t * q.C1))
	v.C1 = ((s * q.C1) + (t * q.C2))
	u.C2 = ((s * u.C1) + (t * v.C1))
	v.C0 = u.C2
	return []QuadSpline{u, v}
}

func subdivideQuadCurve(q QuadCurve, t float64) []QuadCurve {
	var xs []QuadSpline = subdivideQuadSpline(q.X, t)
	_ = xs
	var ys []QuadSpline = subdivideQuadSpline(q.Y, t)
	_ = ys
	var u QuadCurve = QuadCurve{
		X: xs[0],
		Y: ys[0],
	}
	_ = u
	_ = u
	var v QuadCurve = QuadCurve{
		X: xs[1],
		Y: ys[1],
	}
	_ = v
	_ = v
	return []QuadCurve{u, v}
}

func rectsOverlap(xa0 float64, ya0 float64, xa1 float64, ya1 float64, xb0 float64, yb0 float64, xb1 float64, yb1 float64) bool {
	return ((((xb0 <= xa1) && (xa0 <= xb1)) && (yb0 <= ya1)) && (ya0 <= yb1))
}

func testIntersect(p QuadCurve, q QuadCurve, tol float64) map[string]any {
	var pxmin float64 = min3(p.X.C0, p.X.C1, p.X.C2)
	_ = pxmin
	var pymin float64 = min3(p.Y.C0, p.Y.C1, p.Y.C2)
	_ = pymin
	var pxmax float64 = max3(p.X.C0, p.X.C1, p.X.C2)
	_ = pxmax
	var pymax float64 = max3(p.Y.C0, p.Y.C1, p.Y.C2)
	_ = pymax
	var qxmin float64 = min3(q.X.C0, q.X.C1, q.X.C2)
	_ = qxmin
	var qymin float64 = min3(q.Y.C0, q.Y.C1, q.Y.C2)
	_ = qymin
	var qxmax float64 = max3(q.X.C0, q.X.C1, q.X.C2)
	_ = qxmax
	var qymax float64 = max3(q.Y.C0, q.Y.C1, q.Y.C2)
	_ = qymax
	var exclude bool = true
	_ = exclude
	var accept bool = false
	_ = accept
	var inter Point = Point{
		X: 0.0,
		Y: 0.0,
	}
	_ = inter
	if rectsOverlap(pxmin, pymin, pxmax, pymax, qxmin, qymin, qxmax, qymax) {
		exclude = false
		var xmin float64 = maxf(pxmin, qxmin)
		_ = xmin
		var xmax float64 = minf(pxmax, qxmax)
		_ = xmax
		if (xmax - xmin) <= tol {
			var ymin float64 = maxf(pymin, qymin)
			_ = ymin
			var ymax float64 = minf(pymax, qymax)
			_ = ymax
			if (ymax - ymin) <= tol {
				accept = true
				inter.X = (0.5 * (xmin + xmax))
				inter.Y = (0.5 * (ymin + ymax))
			}
		}
	}
	return func(v any) map[string]any {
		if v == nil {
			return nil
		}
		if vv, ok := v.(map[string]any); ok {
			return vv
		}
		return nil
	}(map[string]any{"exclude": exclude, "accept": accept, "intersect": inter})
}

func seemsToBeDuplicate(pts []Point, xy Point, spacing float64) bool {
	var i int = 0
	_ = i
	for i < len(pts) {
		var pt Point = pts[i]
		_ = pt
		if (absf((pt.X - xy.X)) < spacing) && (absf((pt.Y - xy.Y)) < spacing) {
			return true
		}
		i = (i + 1)
	}
	return false
}

func findIntersects(p QuadCurve, q QuadCurve, tol float64, spacing float64) []Point {
	var inters []Point = []Point{}
	_ = inters
	var workload []map[string]QuadCurve = []map[string]QuadCurve{map[string]QuadCurve{"p": p, "q": q}}
	_ = workload
	for len(workload) > 0 {
		var idx int = (len(workload) - 1)
		_ = idx
		var work map[string]QuadCurve = workload[idx]
		_ = work
		workload = workload[:idx]
		var res map[string]any = testIntersect(work["p"], work["q"], tol)
		_ = res
		var excl bool = res["exclude"].(bool)
		_ = excl
		var acc bool = res["accept"].(bool)
		_ = acc
		var inter Point = res["intersect"].(Point)
		_ = inter
		if acc {
			if !seemsToBeDuplicate(func(v any) []Point {
				if v == nil {
					return nil
				}
				if vv, ok := v.([]Point); ok {
					return vv
				}
				if arr, ok := v.([]any); ok {
					if len(arr) == 0 {
						return []Point{}
					}
					out := make([]Point, len(arr))
					for i, x := range arr {
						out[i] = x.(Point)
					}
					return out
				}
				return v.([]Point)
			}(inters), inter, spacing) {
				inters = append(inters, inter)
			}
		} else {
			if !excl {
				var ps []QuadCurve = subdivideQuadCurve(work["p"], 0.5)
				_ = ps
				var qs []QuadCurve = subdivideQuadCurve(work["q"], 0.5)
				_ = qs
				var p0 QuadCurve = ps[0]
				_ = p0
				var p1 QuadCurve = ps[1]
				_ = p1
				var q0 QuadCurve = qs[0]
				_ = q0
				var q1 QuadCurve = qs[1]
				_ = q1
				workload = append(workload, map[string]QuadCurve{"p": p0, "q": q0})
				workload = append(workload, map[string]QuadCurve{"p": p0, "q": q1})
				workload = append(workload, map[string]QuadCurve{"p": p1, "q": q0})
				workload = append(workload, map[string]QuadCurve{"p": p1, "q": q1})
			}
		}
	}
	return inters
}

func mochiMain() {
	var p QuadCurve = QuadCurve{
		X: QuadSpline{
			C0: (0 - 1.0),
			C1: 0.0,
			C2: 1.0,
		},
		Y: QuadSpline{
			C0: 0.0,
			C1: 10.0,
			C2: 0.0,
		},
	}
	_ = p
	var q QuadCurve = QuadCurve{
		X: QuadSpline{
			C0: 2.0,
			C1: (0 - 8.0),
			C2: 2.0,
		},
		Y: QuadSpline{
			C0: 1.0,
			C1: 2.0,
			C2: 3.0,
		},
	}
	_ = q
	var tol float64 = 1e-07
	_ = tol
	var spacing float64 = (tol * 10.0)
	_ = spacing
	var inters []Point = findIntersects(p, q, tol, spacing)
	_ = inters
	var i int = 0
	_ = i
	for i < len(inters) {
		var pt Point = inters[i]
		_ = pt
		fmt.Println((((("(" + fmt.Sprint(pt.X)) + ", ") + fmt.Sprint(pt.Y)) + ")"))
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
