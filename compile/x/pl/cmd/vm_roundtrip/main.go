package main

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	plcode "mochi/compile/x/pl"
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

	var report strings.Builder
	report.WriteString("# Prolog roundtrip VM test failures\n\n")
	for _, src := range files {
		if err := process(src); err != nil {
			report.WriteString(fmt.Sprintf("## %s\n\n```\n%s\n```\n\n", src, err))
		}
	}
	if report.Len() == 0 {
		report.WriteString("All Prolog roundtrip VM tests passed.\n")
	}
	os.WriteFile("compile/x/pl/ERRORS.md", []byte(report.String()), 0644)
}

func process(src string) error {
	base := strings.TrimSuffix(src, ".mochi")
	plPath := base + ".pl.out"

	prog, err := parser.Parse(src)
	if err != nil {
		writeErr(base+".pl.error", err)
		return fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeErr(base+".pl.error", errs[0])
		return fmt.Errorf("type error: %v", errs[0])
	}
	code, err := plcode.New(env).Compile(prog)
	if err != nil {
		writeErr(base+".pl.error", err)
		return fmt.Errorf("compile error: %w", err)
	}
	os.WriteFile(plPath, code, 0644)

	tmpDir, _ := os.MkdirTemp("", "pl-run")
	file := filepath.Join(tmpDir, "main.pl")
	os.WriteFile(file, code, 0644)
	cmd := exec.Command("swipl", "-q", file)
	if fileExists(base + ".in") {
		if f, err := os.Open(base + ".in"); err == nil {
			defer f.Close()
			cmd.Stdin = f
		}
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		writeErr(base+".pl.error", fmt.Errorf("swipl error: %v\n%s", err, out))
		return fmt.Errorf("swipl error: %w\n%s", err, out)
	}
	plOut := strings.TrimSpace(string(out))

	// Run with VM
	p, err := vm.CompileWithSource(prog, env, string(code))
	if err != nil {
		writeErr(base+".vm.error", err)
		return fmt.Errorf("vm compile error: %w", err)
	}
	var in io.Reader = os.Stdin
	if fileExists(base + ".in") {
		if f, err := os.Open(base + ".in"); err == nil {
			defer f.Close()
			in = f
		}
	}
	var buf bytes.Buffer
	m := vm.NewWithIO(p, in, &buf)
	if err := m.Run(); err != nil {
		writeErr(base+".vm.error", err)
		return fmt.Errorf("vm run error: %w", err)
	}
	vmOut := strings.TrimSpace(buf.String())

	if plOut != vmOut {
		writeErr(base+".pl.error", fmt.Errorf("output mismatch\n-- pl --\n%s\n-- vm --\n%s", plOut, vmOut))
		return fmt.Errorf("output mismatch\n-- pl --\n%s\n-- vm --\n%s", plOut, vmOut)
	}

	os.Remove(base + ".pl.error")
	os.Remove(base + ".vm.error")
	return nil
}

func writeErr(path string, err error) {
	os.WriteFile(path, []byte(err.Error()), 0644)
}
