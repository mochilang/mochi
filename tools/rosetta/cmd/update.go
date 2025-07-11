//go:build slow

package main

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"

	"mochi/tools/rosetta"
)

func main() {
	refresh := flag.Bool("refresh", false, "download even if cached")
	flag.Parse()

	tasks, err := rosetta.ListTasks(false)
	if err != nil {
		fmt.Fprintf(os.Stderr, "list tasks: %v\n", err)
		os.Exit(1)
	}

	base := filepath.Join("tests", "rosetta", "x", "Go")
	updated := 0

	for _, task := range tasks {
		names, err := rosetta.ListSources(task, "Go", true)
		if err != nil {
			fmt.Fprintf(os.Stderr, "list sources for %s: %v\n", task, err)
			continue
		}
		if len(names) == 0 {
			continue
		}
		need := false
		if !*refresh {
			for _, name := range names {
				if _, err := os.Stat(filepath.Join(base, name)); err != nil {
					need = true
					break
				}
			}
		} else {
			need = true
		}
		if !need {
			continue
		}
		for _, name := range names {
			if _, err := rosetta.Download(task, "Go", name, true); err != nil {
				fmt.Fprintf(os.Stderr, "download %s/%s: %v\n", task, name, err)
			} else {
				fmt.Printf("downloaded %s/%s\n", task, name)
			}
		}
		updated++
	}

	fmt.Printf("updated %d tasks\n", updated)
}
