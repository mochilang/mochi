package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func minTotalDistance(grid [][]int) int {
	rows := []int{}
	cols := []int{}
	for i := 0; i < len(grid); i++ {
		for j := 0; j < len(grid[i]); j++ {
			if grid[i][j] == 1 {
				rows = append(rows, i)
			}
		}
	}
	for j := 0; j < len(grid[0]); j++ {
		for i := 0; i < len(grid); i++ {
			if grid[i][j] == 1 {
				cols = append(cols, j)
			}
		}
	}
	mr := rows[len(rows)/2]
	mc := cols[len(cols)/2]
	ans := 0
	for _, r := range rows {
		if r > mr {
			ans += r - mr
		} else {
			ans += mr - r
		}
	}
	for _, c := range cols {
		if c > mc {
			ans += c - mc
		} else {
			ans += mc - c
		}
	}
	return ans
}

func main() {
	in := bufio.NewScanner(os.Stdin)
	in.Split(bufio.ScanWords)
	nextInt := func() int {
		in.Scan()
		v, _ := strconv.Atoi(in.Text())
		return v
	}
	if !in.Scan() {
		return
	}
	t, _ := strconv.Atoi(in.Text())
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	for tc := 0; tc < t; tc++ {
		r, c := nextInt(), nextInt()
		grid := make([][]int, r)
		for i := 0; i < r; i++ {
			grid[i] = make([]int, c)
			for j := 0; j < c; j++ {
				grid[i][j] = nextInt()
			}
		}
		if tc > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		fmt.Fprint(out, minTotalDistance(grid))
	}
}
