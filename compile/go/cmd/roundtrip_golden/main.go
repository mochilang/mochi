package main

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	gocode "mochi/compile/go"
	"mochi/parser"
	"mochi/runtime/vm"
	golang "mochi/tools/any2mochi/go"
	"mochi/types"
)

func main() {
	files, err := filepath.Glob("tests/vm/valid/*.mochi")
	if err != nil {
		fmt.Fprintln(os.Stderr, "glob error:", err)
		os.Exit(1)
	}

	var report strings.Builder
	report.WriteString("# Go Round-Trip Failures using runtime/vm\n\n")
	for _, src := range files {
		if err := roundTrip(src); err != nil {
			report.WriteString(fmt.Sprintf("## %s\n\n```\n%s\n```\n\n", src, err))
		}
	}
	if report.Len() == 0 {
		report.WriteString("All Go round-trip tests passed using runtime/vm.\n")
	}
	if err := os.WriteFile("compile/go/ERRORS.md", []byte(report.String()), 0644); err != nil {
		fmt.Fprintln(os.Stderr, "write error:", err)
		os.Exit(1)
	}
}

func roundTrip(src string) error {
	prog, err := parser.Parse(src)
	if err != nil {
		return fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return fmt.Errorf("type error: %v", errs[0])
	}
	code, err := gocode.New(env).Compile(prog)
	if err != nil {
		return fmt.Errorf("compile error: %w", err)
	}
	mochiSrc, err := golang.Convert(string(code))
	if err != nil {
		return fmt.Errorf("convert error: %w", err)
	}
	prog2, err := parser.ParseString(string(mochiSrc))
	if err != nil {
		return fmt.Errorf("roundtrip parse error: %w", err)
	}
	env2 := types.NewEnv(nil)
	if errs := types.Check(prog2, env2); len(errs) > 0 {
		return fmt.Errorf("roundtrip type error: %v", errs[0])
	}
	p2, err := vm.CompileWithSource(prog2, env2, string(mochiSrc))
	if err != nil {
		return fmt.Errorf("vm compile error: %w", err)
	}
	var buf bytes.Buffer
	m := vm.New(p2, &buf)
	if err := m.Run(); err != nil {
		if ve, ok := err.(*vm.VMError); ok {
			return fmt.Errorf("vm run error:\n%s", ve.Format(p2))
		}
		return fmt.Errorf("vm run error: %v", err)
	}
	want, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".out")
	if err != nil {
		return fmt.Errorf("missing golden output: %v", err)
	}
	got := strings.TrimSpace(buf.String())
	if got != strings.TrimSpace(string(want)) {
		return fmt.Errorf("golden mismatch:\n-- got --\n%s\n-- want --\n%s", got, strings.TrimSpace(string(want)))
	}
	return nil
}
