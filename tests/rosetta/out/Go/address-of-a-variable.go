//go:build ignore

package main

import (
	"fmt"
	"strings"
)

func main() {
	var myVar float64 = 3.14
	fmt.Println(strings.TrimRight(strings.Join([]string{fmt.Sprint("value as float:"), fmt.Sprint(myVar)}, " "), " "))
	fmt.Println("address: <not available>")
}
