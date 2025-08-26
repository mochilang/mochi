//go:build slow && ignore

// Solution for SPOJ BULK - The Bulk!
// https://www.spoj.com/problems/BULK
package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
)

type face struct {
	coord int
	poly  [][2]int
}

func main() {
	in := bufio.NewReader(os.Stdin)
	var t int
	fmt.Fscan(in, &t)
	out := bufio.NewWriter(os.Stdout)
	for ; t > 0; t-- {
		var f int
		fmt.Fscan(in, &f)
		xs := map[int]bool{0: true, 1001: true}
		ys := map[int]bool{0: true, 1001: true}
		zs := map[int]bool{0: true, 1001: true}
		var faces []face
		for ; f > 0; f-- {
			var p int
			fmt.Fscan(in, &p)
			pts := make([][3]int, p)
			for i := 0; i < p; i++ {
				fmt.Fscan(in, &pts[i][0], &pts[i][1], &pts[i][2])
				xs[pts[i][0]] = true
				ys[pts[i][1]] = true
				zs[pts[i][2]] = true
			}
			if allEqual(pts, 0) {
				poly := make([][2]int, p)
				for i := 0; i < p; i++ {
					poly[i] = [2]int{pts[i][1], pts[i][2]}
				}
				faces = append(faces, face{pts[0][0], poly})
			}
		}
		xvals := sorted(xs)
		yvals := sorted(ys)
		zvals := sorted(zs)
		vol := 0
		for xi := 0; xi < len(xvals)-1; xi++ {
			xmid := (xvals[xi] + xvals[xi+1]) / 2
			dx := xvals[xi+1] - xvals[xi]
			for yi := 0; yi < len(yvals)-1; yi++ {
				ymid := (yvals[yi] + yvals[yi+1]) / 2
				dy := yvals[yi+1] - yvals[yi]
				for zi := 0; zi < len(zvals)-1; zi++ {
					zmid := (zvals[zi] + zvals[zi+1]) / 2
					dz := zvals[zi+1] - zvals[zi]
					cnt := 0
					for _, fc := range faces {
						if xmid < fc.coord && pointInPoly(fc.poly, ymid, zmid) {
							cnt++
						}
					}
					if cnt%2 == 1 {
						vol += dx * dy * dz
					}
				}
			}
		}
		fmt.Fprintf(out, "The bulk is composed of %d units.\n", vol)
	}
	out.Flush()
}

func allEqual(pts [][3]int, idx int) bool {
	for _, p := range pts[1:] {
		if p[idx] != pts[0][idx] {
			return false
		}
	}
	return true
}

func sorted(m map[int]bool) []int {
	keys := make([]int, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	sort.Ints(keys)
	return keys
}

func pointInPoly(poly [][2]int, y, z int) bool {
	inside := false
	j := len(poly) - 1
	for i := 0; i < len(poly); i++ {
		yi, zi := poly[i][0], poly[i][1]
		yj, zj := poly[j][0], poly[j][1]
		if (zi > z) != (zj > z) &&
			float64(y) < float64(yj-yi)*float64(z-zi)/float64(zj-zi)+float64(yi) {
			inside = !inside
		}
		j = i
	}
	return inside
}
