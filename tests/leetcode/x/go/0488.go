package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func shrinkBoard(s string) string {
	changed := true
	for changed {
		changed = false
		var b strings.Builder
		for i := 0; i < len(s); {
			j := i
			for j < len(s) && s[j] == s[i] {
				j++
			}
			if j-i >= 3 {
				changed = true
			} else {
				b.WriteString(s[i:j])
			}
			i = j
		}
		s = b.String()
	}
	return s
}

func colorID(c byte) int {
	return strings.IndexByte("RYBGW", c)
}

func encodeState(board string, hand [5]int) string {
	return board + "|" + strconv.Itoa(hand[0]) + "," + strconv.Itoa(hand[1]) + "," + strconv.Itoa(hand[2]) + "," + strconv.Itoa(hand[3]) + "," + strconv.Itoa(hand[4])
}

func dfs(board string, hand *[5]int, memo map[string]int) int {
	board = shrinkBoard(board)
	if board == "" {
		return 0
	}
	key := encodeState(board, *hand)
	if v, ok := memo[key]; ok {
		return v
	}
	const inf = int(1e9)
	best := inf
	for i := 0; i < len(board); {
		j := i
		for j < len(board) && board[j] == board[i] {
			j++
		}
		need := 3 - (j - i)
		if need < 0 {
			need = 0
		}
		id := colorID(board[i])
		if hand[id] >= need {
			hand[id] -= need
			sub := dfs(board[:i]+board[j:], hand, memo)
			if sub != inf && need+sub < best {
				best = need + sub
			}
			hand[id] += need
		}
		i = j
	}
	memo[key] = best
	return best
}

func solve(board, handStr string) int {
	var hand [5]int
	for i := 0; i < len(handStr); i++ {
		hand[colorID(handStr[i])]++
	}
	ans := dfs(board, &hand, map[string]int{})
	if ans >= int(1e9) {
		return -1
	}
	return ans
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
		var board, hand string
		fmt.Fscan(in, &board, &hand)
		if tc > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		fmt.Fprint(out, solve(board, hand))
	}
}
