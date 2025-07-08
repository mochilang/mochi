//go:build ignore

package main

import (
	"fmt"
)

func main() {
	m := map[interface{}]interface{}{a: 1, b: 2}
	json(m)
}
