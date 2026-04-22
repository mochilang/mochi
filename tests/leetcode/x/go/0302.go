package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func minArea(image []string, x, y int) int {
	top, bottom := len(image), -1
	left, right := len(image[0]), -1
	for i, row := range image {
		for j, ch := range row {
			if ch == '1' {
				if i < top {
					top = i
				}
				if i > bottom {
					bottom = i
				}
				if j < left {
					left = j
				}
				if j > right {
					right = j
				}
			}
		}
	}
	return (bottom - top + 1) * (right - left + 1)
}

func main() {
	in := bufio.NewScanner(os.Stdin)
	lines := []string{}
	for in.Scan() {
		line := strings.TrimSpace(in.Text())
		if line != "" {
			lines = append(lines, line)
		}
	}
	if len(lines) == 0 {
		return
	}
	t, _ := strconv.Atoi(lines[0])
	idx := 1
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	for tc := 0; tc < t; tc++ {
		parts := strings.Fields(lines[idx])
		idx++
		r, _ := strconv.Atoi(parts[0])
		image := lines[idx : idx+r]
		idx += r
		parts = strings.Fields(lines[idx])
		idx++
		x, _ := strconv.Atoi(parts[0])
		y, _ := strconv.Atoi(parts[1])
		if tc > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		fmt.Fprint(out, minArea(image, x, y))
	}
}
