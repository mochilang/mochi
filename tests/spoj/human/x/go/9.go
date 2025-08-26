//go:build slow && ignore

// Solution for SPOJ DIRVS - Direct Visibility
// https://www.spoj.com/problems/DIRVS
package main

import (
	"bufio"
	"container/list"
	"fmt"
	"math"
	"os"
)

type point struct{ r, c int }

func visible(z [][]int, r, c, br, bc int) bool {
	x0 := float64(c) + 0.5
	y0 := float64(r) + 0.5
	z0 := float64(z[r][c]) + 0.5
	x1 := float64(bc) + 0.5
	y1 := float64(br) + 0.5
	z1 := float64(z[br][bc]) + 0.5

	dx := x1 - x0
	dy := y1 - y0
	dz := z1 - z0
	stepX, stepY, stepZ := 0, 0, 0
	if dx > 0 {
		stepX = 1
	} else if dx < 0 {
		stepX = -1
	}
	if dy > 0 {
		stepY = 1
	} else if dy < 0 {
		stepY = -1
	}
	if dz > 0 {
		stepZ = 1
	} else if dz < 0 {
		stepZ = -1
	}

	x := int(math.Floor(x0))
	y := int(math.Floor(y0))
	zc := int(math.Floor(z0))
	endX := int(math.Floor(x1))
	endY := int(math.Floor(y1))
	endZ := int(math.Floor(z1))

	tDeltaX := math.Inf(1)
	tMaxX := math.Inf(1)
	if stepX != 0 {
		tDeltaX = math.Abs(1 / dx)
		if stepX > 0 {
			tMaxX = (float64(x) + 1 - x0) * tDeltaX
		} else {
			tMaxX = (x0 - float64(x)) * tDeltaX
		}
	}
	tDeltaY := math.Inf(1)
	tMaxY := math.Inf(1)
	if stepY != 0 {
		tDeltaY = math.Abs(1 / dy)
		if stepY > 0 {
			tMaxY = (float64(y) + 1 - y0) * tDeltaY
		} else {
			tMaxY = (y0 - float64(y)) * tDeltaY
		}
	}
	tDeltaZ := math.Inf(1)
	tMaxZ := math.Inf(1)
	if stepZ != 0 {
		tDeltaZ = math.Abs(1 / dz)
		if stepZ > 0 {
			tMaxZ = (float64(zc) + 1 - z0) * tDeltaZ
		} else {
			tMaxZ = (z0 - float64(zc)) * tDeltaZ
		}
	}

	for {
		if zc < z[y][x] {
			return false
		}
		if x == endX && y == endY && zc == endZ {
			return true
		}
		if tMaxX < tMaxY {
			if tMaxX < tMaxZ {
				x += stepX
				tMaxX += tDeltaX
			} else if tMaxX > tMaxZ {
				zc += stepZ
				tMaxZ += tDeltaZ
			} else {
				x += stepX
				zc += stepZ
				tMaxX += tDeltaX
				tMaxZ += tDeltaZ
			}
		} else if tMaxX > tMaxY {
			if tMaxY < tMaxZ {
				y += stepY
				tMaxY += tDeltaY
			} else if tMaxY > tMaxZ {
				zc += stepZ
				tMaxZ += tDeltaZ
			} else {
				y += stepY
				zc += stepZ
				tMaxY += tDeltaY
				tMaxZ += tDeltaZ
			}
		} else {
			if tMaxX < tMaxZ {
				x += stepX
				y += stepY
				tMaxX += tDeltaX
				tMaxY += tDeltaY
			} else if tMaxX > tMaxZ {
				zc += stepZ
				tMaxZ += tDeltaZ
			} else {
				x += stepX
				y += stepY
				zc += stepZ
				tMaxX += tDeltaX
				tMaxY += tDeltaY
				tMaxZ += tDeltaZ
			}
		}
	}
}

func main() {
	in := bufio.NewReader(os.Stdin)
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()

	var T int
	if _, err := fmt.Fscan(in, &T); err != nil {
		return
	}
	dirs := [][2]int{{1, 0}, {-1, 0}, {0, 1}, {0, -1}}
	for ; T > 0; T-- {
		var P, Q int
		fmt.Fscan(in, &P, &Q)
		grid := make([][]int, P)
		for i := 0; i < P; i++ {
			grid[i] = make([]int, Q)
			for j := 0; j < Q; j++ {
				fmt.Fscan(in, &grid[i][j])
			}
		}
		var R1, C1, R2, C2 int
		fmt.Fscan(in, &R1, &C1, &R2, &C2)
		R1--
		C1--
		R2--
		C2--

		vis1 := make([][]bool, P)
		vis2 := make([][]bool, P)
		for i := 0; i < P; i++ {
			vis1[i] = make([]bool, Q)
			vis2[i] = make([]bool, Q)
			for j := 0; j < Q; j++ {
				vis1[i][j] = visible(grid, i, j, R1, C1)
				vis2[i][j] = visible(grid, i, j, R2, C2)
			}
		}

		dist := make([][]int, P)
		for i := range dist {
			dist[i] = make([]int, Q)
			for j := range dist[i] {
				dist[i][j] = -1
			}
		}

		q := list.New()
		if vis1[R1][C1] || vis2[R1][C1] {
			dist[R1][C1] = 0
			q.PushBack(point{R1, C1})
		}

		for q.Len() > 0 {
			e := q.Front()
			q.Remove(e)
			cur := e.Value.(point)
			if cur.r == R2 && cur.c == C2 {
				break
			}
			for _, d := range dirs {
				nr := cur.r + d[0]
				nc := cur.c + d[1]
				if nr < 0 || nr >= P || nc < 0 || nc >= Q {
					continue
				}
				if dist[nr][nc] != -1 {
					continue
				}
				hcur := grid[cur.r][cur.c]
				hnext := grid[nr][nc]
				if hnext-hcur > 1 || hcur-hnext > 3 {
					continue
				}
				if !(vis1[nr][nc] || vis2[nr][nc]) {
					continue
				}
				dist[nr][nc] = dist[cur.r][cur.c] + 1
				q.PushBack(point{nr, nc})
			}
		}
		if dist[R2][C2] == -1 {
			fmt.Fprintln(out, "Mission impossible!")
		} else {
			fmt.Fprintf(out, "The shortest path is %d steps long.\n", dist[R2][C2])
		}
	}
}
