//go:build archived

package main

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	tscode "mochi/archived/ts"
	"mochi/parser"
	"mochi/runtime/mod"
	"mochi/runtime/vm"
	tsconvert "mochi/archived/tools/any2mochi/ts"
	"mochi/types"
)

func main() {
	files, err := filepath.Glob("tests/vm/valid/*.mochi")
	if err != nil {
		fmt.Fprintln(os.Stderr, "glob error:", err)
		os.Exit(1)
	}

	var report strings.Builder
	report.WriteString("# VM Round-trip via TypeScript\n\n")
	for _, src := range files {
		if err := roundtrip(src); err != nil {
			report.WriteString(fmt.Sprintf("## %s\n\n```\n%s\n```\n\n", src, err))
		}
	}
	if report.Len() == 0 {
		report.WriteString("All VM programs passed when round-tripped through TypeScript.\n")
	}
	if err := os.WriteFile("compile/ts/ERRORS.md", []byte(report.String()), 0644); err != nil {
		fmt.Fprintln(os.Stderr, "write error:", err)
		os.Exit(1)
	}
}

func roundtrip(src string) error {
	prog, err := parser.Parse(src)
	if err != nil {
		return fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return fmt.Errorf("type error: %v", errs[0])
	}
	modRoot, _ := mod.FindRoot(filepath.Dir(src))
	if modRoot == "" {
		modRoot = filepath.Dir(src)
	}
	c := tscode.New(env, modRoot)
	code, err := c.Compile(prog)
	if err != nil {
		return fmt.Errorf("compile error: %w", err)
	}
	mochiSrc, err := safeConvert(string(code))
	if err != nil {
		return fmt.Errorf("convert error: %w", err)
	}
	prog2, err := parser.ParseString(string(mochiSrc))
	if err != nil {
		return fmt.Errorf("parse converted error: %w", err)
	}
	env2 := types.NewEnv(nil)
	if errs := types.Check(prog2, env2); len(errs) > 0 {
		return fmt.Errorf("type converted error: %v", errs[0])
	}
	p, err := vm.Compile(prog2, env2)
	if err != nil {
		return fmt.Errorf("vm compile error: %w", err)
	}
	var out bytes.Buffer
	m := vm.New(p, &out)
	if err := m.Run(); err != nil {
		if ve, ok := err.(*vm.VMError); ok {
			return fmt.Errorf("vm run error:\n%s", ve.Format(p))
		}
		return fmt.Errorf("vm run error: %v", err)
	}
	want, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".out")
	if err == nil {
		got := strings.TrimSpace(out.String())
		if got != strings.TrimSpace(string(want)) {
			return fmt.Errorf("golden mismatch:\n-- got --\n%s\n-- want --\n%s", got, strings.TrimSpace(string(want)))
		}
	}
	return nil
}

func safeConvert(src string) ([]byte, error) {
	var out []byte
	var err error
	defer func() {
		if r := recover(); r != nil {
			err = fmt.Errorf("panic: %v", r)
		}
	}()
	out, err = tsconvert.Convert(src)
	return out, err
}
