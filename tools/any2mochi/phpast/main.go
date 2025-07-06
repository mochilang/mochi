package main

import (
	"encoding/json"
	"io/ioutil"
	"os"

	"mochi/tools/any2mochi"
)

func main() {
	data, err := ioutil.ReadAll(os.Stdin)
	if err != nil {
		panic(err)
	}
	prog, err := any2mochi.ParsePhp(string(data))
	if err != nil {
		panic(err)
	}
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	if err := enc.Encode(prog); err != nil {
		panic(err)
	}
}
