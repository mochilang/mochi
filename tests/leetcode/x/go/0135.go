package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func candy(ratings []int) int {
	n := len(ratings)
	candies := make([]int, n)
	for i := range candies {
		candies[i] = 1
	}
	for i := 1; i < n; i++ {
		if ratings[i] > ratings[i-1] {
			candies[i] = candies[i-1] + 1
		}
	}
	for i := n - 2; i >= 0; i-- {
		if ratings[i] > ratings[i+1] && candies[i] < candies[i+1]+1 {
			candies[i] = candies[i+1] + 1
		}
	}
	total := 0
	for _, v := range candies {
		total += v
	}
	return total
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
	for t := 0; t < tc; t++ {
		n, _ := strconv.Atoi(lines[idx])
		idx++
		ratings := make([]int, n)
		for i := 0; i < n; i++ {
			ratings[i], _ = strconv.Atoi(lines[idx])
			idx++
		}
		if t > 0 {
			fmt.Print("\n\n")
		}
		fmt.Print(candy(ratings))
	}
}
