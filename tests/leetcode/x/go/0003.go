package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func longest(s string) int {
	last := map[byte]int{}
	left, best := 0, 0
	for right := 0; right < len(s); right++ {
		ch := s[right]
		if pos, ok := last[ch]; ok && pos >= left {
			left = pos + 1
		}
		last[ch] = right
		if right-left+1 > best {
			best = right - left + 1
		}
	}
	return best
}

func main() {
	reader := bufio.NewReader(os.Stdin)
	var t int
	if _, err := fmt.Fscan(reader, &t); err != nil {
		return
	}
	_, _ = reader.ReadString('\n')
	out := make([]string, 0, t)
	for i := 0; i < t; i++ {
		line, _ := reader.ReadString('\n')
		out = append(out, fmt.Sprint(longest(strings.TrimRight(line, "\r\n"))))
	}
	fmt.Print(strings.Join(out, "\n"))
}
