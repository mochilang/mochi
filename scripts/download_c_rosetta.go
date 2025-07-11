//go:build ignore

package main

import (
	"fmt"
	"mochi/tools/rosetta"
)

// This script downloads C source files for all Rosetta Code tasks
// using the rosetta helper library. Existing files under tests/rosetta/x/C
// are reused.
func main() {
	tasks, err := rosetta.ListTasks(false)
	if err != nil {
		panic(err)
	}
	count := 0
	for i, t := range tasks {
		names, err := rosetta.ListSources(t, "C", false)
		if err != nil {
			continue
		}
		for _, n := range names {
			if _, err := rosetta.Download(t, "C", n, false); err == nil {
				// downloaded or cached
			}
		}
		if len(names) > 0 {
			count++
		}
		if (i+1)%100 == 0 {
			fmt.Printf("processed %d tasks\n", i+1)
		}
	}
	fmt.Printf("C sources available for %d tasks\n", count)
}
