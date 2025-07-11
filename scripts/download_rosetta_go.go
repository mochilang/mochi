//go:build ignore

package main

import (
	"fmt"
	"mochi/tools/rosetta"
	"path/filepath"
)

func main() {
	tasks, err := rosetta.ListTasks(false)
	if err != nil {
		panic(err)
	}
	for i, task := range tasks {
		pattern := filepath.Join("tests", "rosetta", "x", "Go", task+"*.go")
		if matches, _ := filepath.Glob(pattern); len(matches) > 0 {
			continue
		}
		names, err := rosetta.ListSources(task, "Go", false)
		if err != nil || len(names) == 0 {
			continue
		}
		fmt.Printf("%4d %s\n", i+1, task)
		for _, name := range names {
			if _, err := rosetta.Download(task, "Go", name, false); err != nil {
				fmt.Printf("  download error: %v\n", err)
			}
		}
	}
}
