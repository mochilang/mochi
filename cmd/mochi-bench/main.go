package main

import (
	"mochi/bench"
	"os"
)

// mochi-bench either runs the benchmarks or generates the precompiled
// benchmark outputs when the "gen" argument is provided.
func main() {
	if len(os.Args) > 1 && os.Args[1] == "gen" {
		if err := bench.GenerateOutputs("bench/out"); err != nil {
			panic(err)
		}
		return
	}

	bench.Run()
}
