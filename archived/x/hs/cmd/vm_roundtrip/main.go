//go:build archived

package main

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"

	hscode "mochi/archived/x/hs"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func fileExists(path string) bool {
	if _, err := os.Stat(path); err == nil {
		return true
	}
	return false
}

func main() {
	files, err := filepath.Glob("tests/vm/valid/*.mochi")
	if err != nil {
		fmt.Fprintln(os.Stderr, "glob error:", err)
		os.Exit(1)
	}
	if limitStr := os.Getenv("LIMIT"); limitStr != "" {
		if n, err := strconv.Atoi(limitStr); err == nil && n < len(files) {
			files = files[:n]
		}
	}

	var report strings.Builder
	report.WriteString("# Haskell roundtrip VM test failures\n\n")
	for _, src := range files {
		if err := process(src); err != nil {
			report.WriteString(fmt.Sprintf("## %s\n\n```\n%s\n```\n\n", src, err))
		}
	}
	if report.Len() == 0 {
		report.WriteString("All Haskell roundtrip VM tests passed.\n")
	}
	os.WriteFile("compile/x/hs/ERRORS.md", []byte(report.String()), 0644)
}

func process(src string) error {
	base := strings.TrimSuffix(src, ".mochi")
	hsPath := base + ".hs.out"

	prog, err := parser.Parse(src)
	if err != nil {
		return fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return fmt.Errorf("type error: %v", errs[0])
	}
	code, err := hscode.New(env).Compile(prog)
	if err != nil {
		return fmt.Errorf("compile error: %w", err)
	}
	os.WriteFile(hsPath, code, 0644)

	// run Haskell code
	cmd := exec.Command("runhaskell", hsPath)
	var input io.Reader = os.Stdin
	if fileExists(base + ".in") {
		f, err := os.Open(base + ".in")
		if err == nil {
			defer f.Close()
			input = f
		}
	}
	cmd.Stdin = input
	outHS, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("runhaskell error: %w\n%s", err, outHS)
	}
	hsRes := strings.TrimSpace(string(outHS))

	// run VM
	p, err := vm.Compile(prog, env)
	if err != nil {
		return fmt.Errorf("vm compile error: %w", err)
	}
	var vmOut bytes.Buffer
	if fileExists(base + ".in") {
		f, err := os.Open(base + ".in")
		if err == nil {
			defer f.Close()
			m := vm.NewWithIO(p, f, &vmOut)
			if err := m.Run(); err != nil {
				if ve, ok := err.(*vm.VMError); ok {
					return fmt.Errorf("vm run error:\n%s", ve.Format(p))
				}
				return fmt.Errorf("vm run error: %w", err)
			}
		}
	} else {
		m := vm.New(p, &vmOut)
		if err := m.Run(); err != nil {
			if ve, ok := err.(*vm.VMError); ok {
				return fmt.Errorf("vm run error:\n%s", ve.Format(p))
			}
			return fmt.Errorf("vm run error: %w", err)
		}
	}
	vmRes := strings.TrimSpace(vmOut.String())

	if hsRes != vmRes {
		return fmt.Errorf("output mismatch\n-- hs --\n%s\n-- vm --\n%s", hsRes, vmRes)
	}
	return nil
}
