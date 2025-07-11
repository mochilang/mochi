//go:build slow

package main

import (
	"fmt"
	"os"
	"path/filepath"

	rosetta "mochi/tools/rosetta"
)

func repoRoot() string {
	dir, _ := os.Getwd()
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return ""
}

func main() {
	tasks, err := rosetta.ListTasks(false)
	if err != nil {
		fmt.Fprintf(os.Stderr, "list tasks: %v\n", err)
		os.Exit(1)
	}

	root := repoRoot()
	if root == "" {
		fmt.Fprintln(os.Stderr, "repo root not found")
		os.Exit(1)
	}

	for _, task := range tasks {
		names, err := rosetta.ListSources(task, "TypeScript", false)
		if err != nil || len(names) == 0 {
			continue
		}
		for _, name := range names {
			data, err := rosetta.Download(task, "TypeScript", name, false)
			if err != nil {
				fmt.Fprintf(os.Stderr, "download %s/%s: %v\n", task, name, err)
				continue
			}
			destDir := filepath.Join(root, "tests", "rosetta", "x", "Python", task)
			os.MkdirAll(destDir, 0755)
			dest := filepath.Join(destDir, name)
			os.WriteFile(dest, data, 0644)
			fmt.Println("saved", dest)
		}
	}
}
