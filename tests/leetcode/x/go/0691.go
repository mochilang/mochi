package main

import (
	"bufio"
	"fmt"
	"os"
)

func solve(stickers []string, target string) int {
	stickerCounts := make([][26]int, len(stickers))
	for i, sticker := range stickers {
		for _, ch := range sticker {
			stickerCounts[i][ch-'a']++
		}
	}
	targetIdx := make([]int, len(target))
	for i, ch := range target {
		targetIdx[i] = int(ch - 'a')
	}
	fullMask := (1 << len(target)) - 1
	memo := map[int]int{}
	var dfs func(int) int
	dfs = func(mask int) int {
		if mask == fullMask {
			return 0
		}
		if value, ok := memo[mask]; ok {
			return value
		}
		first := 0
		for (mask>>first)&1 == 1 {
			first++
		}
		need := targetIdx[first]
		best := 1 << 30
		for _, counts := range stickerCounts {
			if counts[need] == 0 {
				continue
			}
			remaining := counts
			nextMask := mask
			for i, ch := range targetIdx {
				if ((nextMask>>i)&1) == 0 && remaining[ch] > 0 {
					remaining[ch]--
					nextMask |= 1 << i
				}
			}
			if nextMask != mask {
				candidate := 1 + dfs(nextMask)
				if candidate < best {
					best = candidate
				}
			}
		}
		memo[mask] = best
		return best
	}
	answer := dfs(0)
	if answer >= 1<<30 {
		return -1
	}
	return answer
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
		var n int
		fmt.Fscan(in, &n)
		stickers := make([]string, n)
		for i := 0; i < n; i++ {
			fmt.Fscan(in, &stickers[i])
		}
		var target string
		fmt.Fscan(in, &target)
		if tc > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		fmt.Fprint(out, solve(stickers, target))
	}
}
