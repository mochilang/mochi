package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

const MOD int = 1_000_000_007

func one(c byte) int {
	if c == '*' {
		return 9
	}
	if c == '0' {
		return 0
	}
	return 1
}

func two(a, b byte) int {
	if a == '*' && b == '*' {
		return 15
	}
	if a == '*' {
		if b >= '0' && b <= '6' {
			return 2
		}
		return 1
	}
	if b == '*' {
		if a == '1' {
			return 9
		}
		if a == '2' {
			return 6
		}
		return 0
	}
	val := int(a-'0')*10 + int(b-'0')
	if val >= 10 && val <= 26 {
		return 1
	}
	return 0
}

func solve(s string) int {
	prev2, prev1 := 1, one(s[0])
	for i := 1; i < len(s); i++ {
		cur := (one(s[i])*prev1 + two(s[i-1], s[i])*prev2) % MOD
		prev2, prev1 = prev1, cur
	}
	return prev1
}

func main() {
	in := bufio.NewReader(os.Stdin)
	text, _ := ioReadAll(in)
	lines := []string{}
	for _, line := range strings.Split(string(text), "\n") {
		line = strings.TrimSpace(line)
		if line != "" {
			lines = append(lines, line)
		}
	}
	if len(lines) == 0 {
		return
	}
	var t int
	fmt.Sscan(lines[0], &t)
	out := make([]string, 0, t)
	for i := 1; i <= t; i++ {
		out = append(out, fmt.Sprint(solve(lines[i])))
	}
	fmt.Print(strings.Join(out, "\n\n"))
}

func ioReadAll(r *bufio.Reader) ([]byte, error) {
	buf := make([]byte, 0, 1024)
	for {
		part, err := r.ReadBytes('\n')
		buf = append(buf, part...)
		if err != nil {
			if len(part) == 0 {
				return buf, nil
			}
			return buf, nil
		}
	}
}
