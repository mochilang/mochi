package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
)

type Node struct {
	children map[byte]*Node
	word     string
}

func solve(board [][]byte, words []string) []string {
	root := &Node{children: map[byte]*Node{}}
	for _, word := range words {
		node := root
		for i := 0; i < len(word); i++ {
			ch := word[i]
			if node.children[ch] == nil {
				node.children[ch] = &Node{children: map[byte]*Node{}}
			}
			node = node.children[ch]
		}
		node.word = word
	}
	rows, cols := len(board), len(board[0])
	found := []string{}
	var dfs func(int, int, *Node)
	dfs = func(r, c int, node *Node) {
		ch := board[r][c]
		next := node.children[ch]
		if next == nil {
			return
		}
		if next.word != "" {
			found = append(found, next.word)
			next.word = ""
		}
		board[r][c] = '#'
		if r > 0 && board[r-1][c] != '#' {
			dfs(r-1, c, next)
		}
		if r+1 < rows && board[r+1][c] != '#' {
			dfs(r+1, c, next)
		}
		if c > 0 && board[r][c-1] != '#' {
			dfs(r, c-1, next)
		}
		if c+1 < cols && board[r][c+1] != '#' {
			dfs(r, c+1, next)
		}
		board[r][c] = ch
	}
	for r := 0; r < rows; r++ {
		for c := 0; c < cols; c++ {
			if root.children[board[r][c]] != nil {
				dfs(r, c, root)
			}
		}
	}
	sort.Strings(found)
	return found
}

func main() {
	sc := bufio.NewScanner(os.Stdin)
	sc.Split(bufio.ScanWords)
	next := func() string { sc.Scan(); return sc.Text() }
	if !sc.Scan() {
		return
	}
	t, _ := strconv.Atoi(sc.Text())
	w := bufio.NewWriter(os.Stdout)
	defer w.Flush()
	for tc := 0; tc < t; tc++ {
		rows, _ := strconv.Atoi(next())
		cols, _ := strconv.Atoi(next())
		board := make([][]byte, rows)
		for i := 0; i < rows; i++ {
			board[i] = []byte(next())
		}
		n, _ := strconv.Atoi(next())
		words := make([]string, n)
		for i := 0; i < n; i++ {
			words[i] = next()
		}
		ans := solve(board, words)
		if tc > 0 {
			fmt.Fprint(w, "\n\n")
		}
		fmt.Fprint(w, len(ans))
		for _, s := range ans {
			fmt.Fprint(w, "\n", s)
		}
		_ = cols
	}
}
