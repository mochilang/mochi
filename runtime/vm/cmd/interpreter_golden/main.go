package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

func fileExists(path string) bool {
	if _, err := os.Stat(path); err == nil {
		return true
	}
	return false
}

func main() {
	files, err := filepath.Glob("tests/interpreter/valid/*.mochi")
	if err != nil {
		fmt.Fprintln(os.Stderr, "glob error:", err)
		os.Exit(1)
	}

	var report strings.Builder
	report.WriteString("# Interpreter Golden Test Failures using runtime/vm\n\n")
	for _, src := range files {
		if err := run(src); err != nil {
			report.WriteString(fmt.Sprintf("## %s\n\n```\n%s\n```\n\n", src, err))
		}
	}
	if report.Len() == 0 {
		report.WriteString("All interpreter golden tests passed using runtime/vm.\n")
	}
	if err := os.WriteFile("runtime/vm/ERRORS.md", []byte(report.String()), 0644); err != nil {
		fmt.Fprintln(os.Stderr, "write error:", err)
		os.Exit(1)
	}
}

func run(src string) error {
	cmd := exec.Command("go", "run", "./cmd/mochi", "run", src)
	if inPath := strings.TrimSuffix(src, ".mochi") + ".in"; fileExists(inPath) {
		f, err := os.Open(inPath)
		if err != nil {
			return fmt.Errorf("open input: %w", err)
		}
		defer f.Close()
		cmd.Stdin = f
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		msg := string(bytes.TrimSpace(out))
		lines := strings.Split(msg, "\n")
		if len(lines) > 10 {
			msg = strings.Join(lines[:10], "\n") + "\n..."
		}
		return fmt.Errorf("%s", msg)
	}
	want, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".out")
	if err != nil {
		return fmt.Errorf("missing golden output: %v", err)
	}
	got := strings.TrimSpace(string(out))
	if got != strings.TrimSpace(string(want)) {
		return fmt.Errorf("golden mismatch:\n-- got --\n%s\n-- want --\n%s", got, strings.TrimSpace(string(want)))
	}
	return nil
}
