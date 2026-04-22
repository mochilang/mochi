package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

type MedianFinder struct {
	data []int
}

func (m *MedianFinder) addNum(num int) {
	pos := sort.SearchInts(m.data, num)
	m.data = append(m.data, 0)
	copy(m.data[pos+1:], m.data[pos:])
	m.data[pos] = num
}

func (m *MedianFinder) findMedian() float64 {
	n := len(m.data)
	if n%2 == 1 {
		return float64(m.data[n/2])
	}
	return float64(m.data[n/2-1]+m.data[n/2]) / 2.0
}

func main() {
	sc := bufio.NewScanner(os.Stdin)
	var lines []string
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		if line != "" {
			lines = append(lines, line)
		}
	}
	if len(lines) == 0 {
		return
	}
	t, _ := strconv.Atoi(lines[0])
	idx := 1
	var blocks []string
	for tc := 0; tc < t; tc++ {
		m, _ := strconv.Atoi(lines[idx])
		idx++
		mf := MedianFinder{}
		var out []string
		for i := 0; i < m; i++ {
			parts := strings.Fields(lines[idx])
			idx++
			if parts[0] == "addNum" {
				v, _ := strconv.Atoi(parts[1])
				mf.addNum(v)
			} else {
				out = append(out, fmt.Sprintf("%.1f", mf.findMedian()))
			}
		}
		blocks = append(blocks, strings.Join(out, "\n"))
	}
	fmt.Print(strings.Join(blocks, "\n\n"))
}
