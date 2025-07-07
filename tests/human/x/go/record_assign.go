//go:build ignore

package main

import "fmt"

type Counter struct {
	N int
}

func inc(c *Counter) {
	c.N = c.N + 1
}

func main() {
	c := &Counter{N: 0}
	inc(c)
	fmt.Println(c.N)
}
