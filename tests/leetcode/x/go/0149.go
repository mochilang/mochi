package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Point struct{ x, y int }

func gcd(a, b int) int {
	for b != 0 {
		a, b = b, a%b
	}
	if a < 0 {
		return -a
	}
	return a
}

func maxPoints(points []Point) int {
	n := len(points)
	if n <= 2 {
		return n
	}
	best := 0
	for i := 0; i < n; i++ {
		slopes := map[string]int{}
		local := 0
		for j := i + 1; j < n; j++ {
			dx := points[j].x - points[i].x
			dy := points[j].y - points[i].y
			g := gcd(dx, dy)
			dx /= g
			dy /= g
			if dx < 0 {
				dx = -dx
				dy = -dy
			} else if dx == 0 {
				dy = 1
			} else if dy == 0 {
				dx = 1
			}
			key := strconv.Itoa(dy) + "/" + strconv.Itoa(dx)
			slopes[key]++
			if slopes[key] > local {
				local = slopes[key]
			}
		}
		if local+1 > best {
			best = local + 1
		}
	}
	return best
}

func main() {
	in := bufio.NewScanner(os.Stdin)
	var lines []string
	for in.Scan() {
		lines = append(lines, in.Text())
	}
	if len(lines) == 0 {
		return
	}
	tc, _ := strconv.Atoi(lines[0])
	idx := 1
	out := make([]string, 0, tc)
	for t := 0; t < tc; t++ {
		n, _ := strconv.Atoi(lines[idx])
		idx++
		points := make([]Point, n)
		for i := 0; i < n; i++ {
			parts := strings.Fields(lines[idx])
			idx++
			x, _ := strconv.Atoi(parts[0])
			y, _ := strconv.Atoi(parts[1])
			points[i] = Point{x, y}
		}
		out = append(out, strconv.Itoa(maxPoints(points)))
	}
	fmt.Print(strings.Join(out, "\n\n"))
}
