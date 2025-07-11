//go:build ignore

package main

import (
    "fmt"
    "path/filepath"
    "mochi/tools/rosetta"
)

// This script downloads Python source files for the first 500 Rosetta Code tasks
// using the rosetta helper library. Existing files under tests/rosetta/x/Python
// are reused.
func main() {
    tasks, err := rosetta.ListTasks(false)
    if err != nil {
        panic(err)
    }
    limit := 500
    if len(tasks) < limit {
        limit = len(tasks)
    }
    for i := 0; i < limit; i++ {
        task := tasks[i]
        pattern := filepath.Join("tests", "rosetta", "x", "Python", task, "*")
        if matches, _ := filepath.Glob(pattern); len(matches) > 0 {
            continue
        }
        names, err := rosetta.ListSources(task, "Python", false)
        if err != nil || len(names) == 0 {
            continue
        }
        fmt.Printf("%4d %s\n", i+1, task)
        for _, name := range names {
            if _, err := rosetta.Download(task, "Python", name, false); err != nil {
                fmt.Printf("  download error: %v\n", err)
            }
        }
    }
}
