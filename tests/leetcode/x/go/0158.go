package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func quote(s string) string {
	return `"` + s + `"`
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
	var out []string
	for t := 0; t < tc; t++ {
		data := lines[idx]
		idx++
		q, _ := strconv.Atoi(lines[idx])
		idx++
		pos := 0
		block := []string{strconv.Itoa(q)}
		for i := 0; i < q; i++ {
			n, _ := strconv.Atoi(lines[idx])
			idx++
			end := pos + n
			if end > len(data) {
				end = len(data)
			}
			part := data[pos:end]
			pos = end
			block = append(block, quote(part))
		}
		out = append(out, strings.Join(block, "\n"))
	}
	fmt.Print(strings.Join(out, "\n\n"))
}
