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
	for _, lang := range langs {
		df := sandbox.DockerfileFor(lang)
		path := filepath.Join(outDir, lang+".Dockerfile")
		if err := os.WriteFile(path, []byte(df), 0644); err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
	}
}
