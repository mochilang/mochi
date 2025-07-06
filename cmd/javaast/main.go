package main

import (
	"io/ioutil"
	"os"
)

func main() {
	if _, err := ioutil.ReadAll(os.Stdin); err != nil {
		panic(err)
	}
	os.Stderr.WriteString("java parsing not supported\n")
	os.Exit(1)
}
