package main

import (
	"bufio"
	"fmt"
	"os"
)

type Robot struct {
	grid    []string
	r, c    int
	dir     int
	cleaned map[[2]int]bool
}

var dr = [4]int{-1, 0, 1, 0}
var dc = [4]int{0, 1, 0, -1}

func NewRobot(grid []string, sr, sc int) *Robot {
	return &Robot{grid: grid, r: sr, c: sc, cleaned: map[[2]int]bool{}}
}

func (r *Robot) Move() bool {
	nr, nc := r.r+dr[r.dir], r.c+dc[r.dir]
	if nr < 0 || nr >= len(r.grid) || nc < 0 || nc >= len(r.grid[0]) || r.grid[nr][nc] != '1' {
		return false
	}
	r.r, r.c = nr, nc
	return true
}

func (r *Robot) TurnRight() { r.dir = (r.dir + 1) % 4 }
func (r *Robot) Clean()     { r.cleaned[[2]int{r.r, r.c}] = true }

func goBack(robot *Robot) {
	robot.TurnRight()
	robot.TurnRight()
	robot.Move()
	robot.TurnRight()
	robot.TurnRight()
}

func dfs(robot *Robot, x, y, dir int, vis map[[2]int]bool) {
	vis[[2]int{x, y}] = true
	robot.Clean()
	for i := 0; i < 4; i++ {
		nd := (dir + i) % 4
		nx, ny := x+dr[nd], y+dc[nd]
		if !vis[[2]int{nx, ny}] && robot.Move() {
			dfs(robot, nx, ny, nd, vis)
			goBack(robot)
		}
		robot.TurnRight()
	}
}

func solve(grid []string, sr, sc int) int {
	robot := NewRobot(grid, sr, sc)
	dfs(robot, 0, 0, 0, map[[2]int]bool{})
	return len(robot.cleaned)
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
		var n, m, sr, sc int
		fmt.Fscan(in, &n, &m, &sr, &sc)
		grid := make([]string, n)
		for i := 0; i < n; i++ {
			fmt.Fscan(in, &grid[i])
		}
		if tc > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		fmt.Fprint(out, solve(grid, sr, sc))
	}
}
