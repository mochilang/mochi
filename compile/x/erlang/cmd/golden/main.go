package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	erlcode "mochi/compile/x/erlang"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func main() {
	// Provide a fake erlfmt to avoid attempts to install it.
	tmpFmt, _ := os.MkdirTemp("", "erlfmt")
	fake := filepath.Join(tmpFmt, "erlfmt")
	_ = os.WriteFile(fake, []byte("#!/bin/sh\ncat"), 0755)
	oldPath := os.Getenv("PATH")
	os.Setenv("PATH", tmpFmt+":"+oldPath)
	defer os.Setenv("PATH", oldPath)

	files, err := filepath.Glob("tests/compiler/erl/dataset.mochi")
	if err != nil {
		fmt.Fprintln(os.Stderr, "glob error:", err)
		os.Exit(1)
	}

	var report strings.Builder
	report.WriteString("# Compiler Golden Test Failures using runtime/vm\n\n")
	for _, src := range files {
		if err := run(src); err != nil {
			report.WriteString(fmt.Sprintf("## %s\n\n```\n%s\n```\n\n", src, err))
		}
	}
	if report.Len() == 0 {
		report.WriteString("All compiler golden tests passed using runtime/vm.\n")
	}
	os.WriteFile("compile/x/erlang/ERRORS.md", []byte(report.String()), 0644)
}

func run(src string) error {
	prog, err := parser.Parse(src)
	if err != nil {
		return fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return fmt.Errorf("type error: %v", errs[0])
	}

	p, err := vm.Compile(prog, env)
	if err != nil {
		return fmt.Errorf("vm compile error: %w", err)
	}
	var wantBuf bytes.Buffer
	m := vm.New(p, &wantBuf)
	if err := m.Run(); err != nil {
		return fmt.Errorf("vm run error: %w", err)
	}
	want := strings.TrimSpace(wantBuf.String())

	code, err := erlcode.New(env).Compile(prog)
	if err != nil {
		return fmt.Errorf("compile error: %w", err)
	}
	dir, err := os.MkdirTemp("", "erl-compile")
	if err != nil {
		return fmt.Errorf("temp dir: %w", err)
	}
	defer os.RemoveAll(dir)
	file := filepath.Join(dir, "main.erl")
	if err := os.WriteFile(file, code, 0755); err != nil {
		return fmt.Errorf("write error: %w", err)
	}
	cmd := exec.Command("escript", file)
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("escript error: %w\n%s", err, out)
	}
	got := strings.TrimSpace(string(out))
	if got != want {
		return fmt.Errorf("output mismatch:\n-- got --\n%s\n-- want --\n%s", got, want)
	}
	return nil
}
