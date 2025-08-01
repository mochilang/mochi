//go:build ignore

// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z

package main

import (
	"fmt"
	"strings"
	"time"
)

type v map[string]any

func main() {
	nPts := 100
	rMin := 10
	rMax := 15
	span := ((rMax + 1) + rMax)
	var poss [][]int = [][]int{}
	min2 := (rMin * rMin)
	max2 := (rMax * rMax)
	y := -rMax
	for y <= rMax {
		x := -rMax
		for x <= rMax {
			r2 := ((x * x) + (y * y))
			if (r2 >= min2) && (r2 <= max2) {
				poss = append(_toAnySlice(poss), any([]int{x, y}))
			}
			x = (x + 1)
		}
		y = (y + 1)
	}
	fmt.Println(strings.TrimSuffix(fmt.Sprintln(any(fmt.Sprint(any(len(any(poss))))+" possible points")), "\n"))
	var rows [][]string = [][]string{}
	r := 0
	for r < span {
		var row []string = []string{}
		c := 0
		for c < (span * 2) {
			row = append(_toAnySlice(row), any(" "))
			c = (c + 1)
		}
		rows = append(_toAnySlice(rows), any(row))
		r = (r + 1)
	}
	u := 0
	var seen map[string]bool = map[string]bool{}
	n := 0
	for n < nPts {
		i := (int64(time.Now().UnixNano()) % int64(len(any(poss))))
		x := poss[i][0]
		yy := poss[i][1]
		row := (yy + rMax)
		col := ((x + rMax) * 2)
		rows[row][col] = "*"
		key := fmt.Sprint(any(row)) + "," + fmt.Sprint(any(col))
		if !(seen[key]) {
			seen[key] = true
			u = (u + 1)
		}
		n = (n + 1)
	}
	i2 := 0
	for i2 < span {
		line := ""
		j := 0
		for j < (span * 2) {
			line = line + rows[i2][j]
			j = (j + 1)
		}
		fmt.Println(strings.TrimSuffix(fmt.Sprintln(any(line)), "\n"))
		i2 = (i2 + 1)
	}
	fmt.Println(strings.TrimSuffix(fmt.Sprintln(any(fmt.Sprint(any(u))+" unique points")), "\n"))
}

func _toAnySlice[T any](s []T) []any {
	out := make([]any, len(s))
	for i, v := range s {
		out[i] = v
	}
	return out
}
