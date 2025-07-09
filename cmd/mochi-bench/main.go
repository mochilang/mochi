//go:build slow

package main

import (
	"mochi/bench"
)

// mochi-bench generates the precompiled benchmark sources and then runs the benchmarks.
func main() {
	if err := bench.GenerateOutputs("bench/out"); err != nil {
		panic(err)
	}

	bench.Run()
}
