package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strings"
)

type point struct{ x, y int }

func canonical(cells []point) string {
	transforms := make([][]point, 8)
	for _, p := range cells {
		variants := [8]point{
			{p.x, p.y},
			{p.x, -p.y},
			{-p.x, p.y},
			{-p.x, -p.y},
			{p.y, p.x},
			{p.y, -p.x},
			{-p.y, p.x},
			{-p.y, -p.x},
		}
		for i, q := range variants {
			transforms[i] = append(transforms[i], q)
		}
	}
	best := ""
	for _, points := range transforms {
		minX, minY := points[0].x, points[0].y
		for _, p := range points {
			if p.x < minX {
				minX = p.x
			}
			if p.y < minY {
				minY = p.y
			}
		}
		norm := make([]point, len(points))
		for i, p := range points {
			norm[i] = point{p.x - minX, p.y - minY}
		}
		sort.Slice(norm, func(i, j int) bool {
			if norm[i].x != norm[j].x {
				return norm[i].x < norm[j].x
			}
			return norm[i].y < norm[j].y
		})
		parts := make([]string, len(norm))
		for i, p := range norm {
			parts[i] = fmt.Sprintf("%d:%d", p.x, p.y)
		}
		key := strings.Join(parts, "|")
		if best == "" || key < best {
			best = key
		}
	}
	return best
}

func solve(grid [][]int) int {
	rows, cols := len(grid), len(grid[0])
	seen := make([][]bool, rows)
	for i := range seen {
		seen[i] = make([]bool, cols)
	}
	shapes := map[string]bool{}
	dirs := [][2]int{{1, 0}, {-1, 0}, {0, 1}, {0, -1}}
	for r := 0; r < rows; r++ {
		for c := 0; c < cols; c++ {
			if grid[r][c] == 0 || seen[r][c] {
				continue
			}
			stack := []point{{r, c}}
			seen[r][c] = true
			cells := []point{}
			for len(stack) > 0 {
				p := stack[len(stack)-1]
				stack = stack[:len(stack)-1]
				cells = append(cells, point{p.x - r, p.y - c})
				for _, d := range dirs {
					nx, ny := p.x+d[0], p.y+d[1]
					if nx >= 0 && nx < rows && ny >= 0 && ny < cols && grid[nx][ny] == 1 && !seen[nx][ny] {
						seen[nx][ny] = true
						stack = append(stack, point{nx, ny})
					}
				}
			}
			shapes[canonical(cells)] = true
		}
	}
	return len(shapes)
}

func main() {
	in := bufio.NewReader(os.Stdin)
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	var t int
	if _, err := fmt.Fscan(in, &t); err != nil {
		return
	}
	for tc := 0; tc < t; tc++ {
		var rows, cols int
		fmt.Fscan(in, &rows, &cols)
		grid := make([][]int, rows)
		for i := 0; i < rows; i++ {
			grid[i] = make([]int, cols)
			for j := 0; j < cols; j++ {
				fmt.Fscan(in, &grid[i][j])
			}
		}
		if tc > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		fmt.Fprint(out, solve(grid))
	}
}
