//go:build ignore

package main

import (
	"fmt"
)

type Counter struct {
	N int `json:"n"`
}

// line 3
func inc(c Counter) {
	c.N = (c.N + 1)
}

var c Counter

func main() {
	c = Counter{N: 0}
	inc(&c)
	fmt.Println(c.N)
}
