//go:build ignore
// +build ignore

// Rosetta Code task 364: Fibonacci n-step number sequences

package main

import "fmt"

func g(i []int, c chan<- int) {
	var sum int
	b := append([]int(nil), i...) // make a copy
	for _, t := range b {
		c <- t
		sum += t
	}
	for {
		for j, t := range b {
			c <- sum
			b[j], sum = sum, sum+sum-t
		}
	}
}

func main() {
	for _, s := range [...]struct {
		seq string
		i   []int
	}{
		{"Fibonacci", []int{1, 1}},
		{"Tribonacci", []int{1, 1, 2}},
		{"Tetranacci", []int{1, 1, 2, 4}},
		{"Lucas", []int{2, 1}},
	} {
		fmt.Printf("%10s:", s.seq)
		c := make(chan int)
		// Note/warning: these goroutines are leaked.
		go g(s.i, c)
		for j := 0; j < 10; j++ {
			fmt.Print(" ", <-c)
		}
		fmt.Println()
	}
}
