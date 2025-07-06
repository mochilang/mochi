package main

import (
	"encoding/json"
	"io/ioutil"
	"os"

	am "mochi/tools/any2mochi"
)

func main() {
	data, err := ioutil.ReadAll(os.Stdin)
	if err != nil {
		panic(err)
	}
	os.Stderr.WriteString("java parsing not supported\n")
	os.Exit(1)
}
