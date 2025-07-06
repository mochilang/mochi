package main

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	zigcode "mochi/compile/x/zig"
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

func run(src string) error {
	prog, err := parser.Parse(src)
	if err != nil {
		return fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return fmt.Errorf("type error: %v", errs[0])
	}
	code, err := zigcode.New(env).Compile(prog)
	if err != nil {
		return fmt.Errorf("compile error: %w", err)
	}
	zigc, err := zigcode.EnsureZig()
	if err != nil {
		return err
	}
	tmpDir, err := os.MkdirTemp("", "zigprog")
	if err != nil {
		return err
	}
	defer os.RemoveAll(tmpDir)
	file := filepath.Join(tmpDir, "prog.zig")
	if err := os.WriteFile(file, code, 0644); err != nil {
		return fmt.Errorf("write error: %w", err)
	}
	exe := filepath.Join(tmpDir, "prog")
	if out, err := exec.Command(zigc, "build-exe", file, "-O", "ReleaseSafe", "-femit-bin="+exe).CombinedOutput(); err != nil {
		return fmt.Errorf("zig build error: %v\n%s", err, out)
	}
	cmd := exec.Command(exe)
	if fileExists(strings.TrimSuffix(src, ".mochi") + ".in") {
		data, _ := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in")
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("run error: %v\n%s", err, out)
	}
	compiled := strings.TrimSpace(string(out))

	p, err := vm.Compile(prog, env)
	if err != nil {
		return fmt.Errorf("vm compile error: %w", err)
	}
	var in io.Reader = os.Stdin
	if fileExists(strings.TrimSuffix(src, ".mochi") + ".in") {
		f, err := os.Open(strings.TrimSuffix(src, ".mochi") + ".in")
		if err == nil {
			defer f.Close()
			in = f
		}
	}
	var buf bytes.Buffer
	m := vm.NewWithIO(p, in, &buf)
	if err := m.Run(); err != nil {
		if ve, ok := err.(*vm.VMError); ok {
			return fmt.Errorf("vm run error:\n%s", ve.Format(p))
		}
		return fmt.Errorf("vm run error: %v", err)
	}
	vmOut := strings.TrimSpace(buf.String())
	if compiled != vmOut {
		return fmt.Errorf("output mismatch\n-- compiled --\n%s\n-- vm --\n%s", compiled, vmOut)
	}
	return nil
}

func main() {
	files, err := filepath.Glob("tests/vm/valid/*.mochi")
	if err != nil {
		fmt.Fprintln(os.Stderr, "glob error:", err)
		os.Exit(1)
	}
	var report strings.Builder
	report.WriteString("# Zig Compiler VM Test Report\n\n")
	for _, src := range files {
		if err := run(src); err != nil {
			report.WriteString(fmt.Sprintf("## %s\n\n```\n%s\n```\n\n", src, err))
		}
	}
	if report.Len() == 0 {
		report.WriteString("All compiler tests passed.\n")
	}
	if err := os.WriteFile("compile/x/zig/ERRORS.md", []byte(report.String()), 0644); err != nil {
		fmt.Fprintln(os.Stderr, "write error:", err)
		os.Exit(1)
	}
}
