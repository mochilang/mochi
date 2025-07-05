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
	lines, err := am.ParseJavaInternal(string(data))
	if err != nil {
		// print error and exit nonzero
		os.Stderr.WriteString(err.Error())
		os.Exit(1)
	}
	json.NewEncoder(os.Stdout).Encode(struct {
		Lines []string `json:"lines"`
	}{Lines: lines})
}
