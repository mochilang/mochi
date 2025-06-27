//go:build ignore

package main

import (
	"fmt"

	"mochi/tools/cosmo"
)

func main() {
	out, err := cosmo.CompileAndRun(`#include <stdio.h>
int main(){printf("%d",5*5);}`)
	if err != nil {
		panic(err)
	}
	fmt.Println(out)
}
