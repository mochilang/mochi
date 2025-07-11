//go:build ignore

package main

import (
	"fmt"
	"strings"
)

func main() {
	fmt.Println(strings.TrimSuffix(strings.TrimPrefix(fmt.Sprint([]int{1, 2, 3}[1:3]), "["), "]"))
	fmt.Println(strings.TrimSuffix(strings.TrimPrefix(fmt.Sprint([]int{1, 2, 3}[0:2]), "["), "]"))
	fmt.Println(string([]rune("hello")[1:4]))
}
