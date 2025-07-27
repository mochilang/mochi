//go:build ignore
// +build ignore

package main

import (
	"fmt"
	"os"
)

func bottles(n int) string {
	switch n {
	case 0:
		return "No more bottles"
	case 1:
		return "1 bottle"
	default:
		return fmt.Sprintf("%d bottles", n)
	}
}

func sing99() {
	for i := 99; i > 0; i-- {
		fmt.Printf("%s of beer on the wall\n", bottles(i))
		fmt.Printf("%s of beer\n", bottles(i))
		fmt.Println("Take one down, pass it around")
		fmt.Printf("%s of beer on the wall\n", bottles(i-1))
		if i > 1 {
			fmt.Println()
		}
	}
}

func execute(code string) {
	acc := 0
	for _, ch := range code {
		switch ch {
		case 'H':
			fmt.Println("Hello, World!")
		case 'Q':
			fmt.Println(code)
		case '9':
			sing99()
		case '+':
			acc++
		}
	}
	_ = acc
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "usage: execute-hq9+ CODE")
		return
	}
	execute(os.Args[1])
}
