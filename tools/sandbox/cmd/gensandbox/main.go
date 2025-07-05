package main

import (
	"fmt"
	"os"
	"path/filepath"

	"mochi/tools/sandbox"
)

func main() {
	langs, err := sandbox.ListLanguages()
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	outDir := filepath.Join("tools", "sandbox", "dockerfiles")
	if err := os.MkdirAll(outDir, 0755); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	runnerDir := filepath.Join("tools", "sandbox", "runner")
	if err := os.MkdirAll(runnerDir, 0755); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	for _, lang := range langs {
		df := sandbox.DockerfileFor(lang)
		path := filepath.Join(outDir, lang+".Dockerfile")
		if err := os.WriteFile(path, []byte(df), 0644); err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
		rDir := filepath.Join(runnerDir, lang)
		if err := os.MkdirAll(rDir, 0755); err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
		mainSrc := sandbox.RunnerMainFor(lang)
		if err := os.WriteFile(filepath.Join(rDir, "main.go"), []byte(mainSrc), 0644); err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
	}
}
