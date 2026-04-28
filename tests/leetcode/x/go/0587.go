package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strings"
)

type Point struct {
	x int
	y int
}

func cross(o, a, b Point) int {
	return (a.x-o.x)*(b.y-o.y) - (a.y-o.y)*(b.x-o.x)
}

func solve(points []Point) []Point {
	sort.Slice(points, func(i, j int) bool {
		if points[i].x != points[j].x {
			return points[i].x < points[j].x
		}
		return points[i].y < points[j].y
	})
	pts := make([]Point, 0, len(points))
	for _, p := range points {
		if len(pts) == 0 || pts[len(pts)-1] != p {
			pts = append(pts, p)
		}
	}
	if len(pts) <= 1 {
		return pts
	}
	lower := make([]Point, 0)
	for _, p := range pts {
		for len(lower) >= 2 && cross(lower[len(lower)-2], lower[len(lower)-1], p) < 0 {
			lower = lower[:len(lower)-1]
		}
		lower = append(lower, p)
	}
	upper := make([]Point, 0)
	for i := len(pts) - 1; i >= 0; i-- {
		p := pts[i]
		for len(upper) >= 2 && cross(upper[len(upper)-2], upper[len(upper)-1], p) < 0 {
			upper = upper[:len(upper)-1]
		}
		upper = append(upper, p)
	}
	seen := map[Point]bool{}
	out := make([]Point, 0)
	for _, p := range append(lower, upper...) {
		if !seen[p] {
			seen[p] = true
			out = append(out, p)
		}
	}
	sort.Slice(out, func(i, j int) bool {
		if out[i].x != out[j].x {
			return out[i].x < out[j].x
		}
		return out[i].y < out[j].y
	})
	return out
}

func main() {
	in := bufio.NewReader(os.Stdin)
	var t int
	if _, err := fmt.Fscan(in, &t); err != nil {
		return
	}
	blocks := make([]string, 0)
	for tc := 0; tc < t; tc++ {
		var n int
		fmt.Fscan(in, &n)
		pts := make([]Point, n)
		for i := 0; i < n; i++ {
			fmt.Fscan(in, &pts[i].x, &pts[i].y)
		}
		hull := solve(pts)
		if tc > 0 {
			blocks = append(blocks, "")
		}
		blocks = append(blocks, fmt.Sprint(len(hull)))
		for _, p := range hull {
			blocks = append(blocks, fmt.Sprintf("%d %d", p.x, p.y))
		}
	}
	fmt.Print(strings.Join(blocks, "\n"))
}
