package main

import (
	"encoding/json"
	"io/ioutil"
	"os"

	php "mochi/archived/tools/any2mochi/x/php"
)

func main() {
	data, err := ioutil.ReadAll(os.Stdin)
	if err != nil {
		panic(err)
	}
	prog, err := php.Parse(string(data))
	if err != nil {
		panic(err)
	}
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	if err := enc.Encode(prog); err != nil {
		panic(err)
	}
}
