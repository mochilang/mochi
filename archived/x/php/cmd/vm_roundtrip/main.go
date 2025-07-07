//go:build archived

package main

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	phpcode "mochi/archived/x/php"
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
	if err := phpcode.EnsurePHP(); err != nil {
		fmt.Fprintln(os.Stderr, "php not installed:", err)
		os.Exit(1)
	}
	files, err := filepath.Glob("tests/vm/valid/*.mochi")
	if err != nil {
		fmt.Fprintln(os.Stderr, "glob error:", err)
		os.Exit(1)
	}

	var report strings.Builder
	report.WriteString("# PHP roundtrip VM test failures\n\n")
	for _, src := range files {
		if err := process(src); err != nil {
			report.WriteString(fmt.Sprintf("## %s\n\n```\n%s\n```\n\n", src, err))
		}
	}
	if report.Len() == 0 {
		report.WriteString("All PHP roundtrip VM tests passed.\n")
	}
	if err := os.WriteFile("compile/x/php/ERRORS.md", []byte(report.String()), 0644); err != nil {
		fmt.Fprintln(os.Stderr, "write error:", err)
		os.Exit(1)
	}
}

func process(src string) error {
	base := strings.TrimSuffix(src, ".mochi")
	phpPath := base + ".php.out"

	prog, err := parser.Parse(src)
	if err != nil {
		writeErr(base+".php.error", err)
		return fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeErr(base+".php.error", errs[0])
		return fmt.Errorf("type error: %v", errs[0])
	}
	code, err := phpcode.New(env).Compile(prog)
	if err != nil {
		writeErr(base+".php.error", err)
		return fmt.Errorf("compile error: %w", err)
	}
	_ = os.WriteFile(phpPath, code, 0644)

	tmpDir, err := os.MkdirTemp("", "php_run")
	if err != nil {
		return err
	}
	defer os.RemoveAll(tmpDir)
	file := filepath.Join(tmpDir, "main.php")
	if err := os.WriteFile(file, code, 0644); err != nil {
		return fmt.Errorf("write temp php: %w", err)
	}
	cmd := exec.Command("php", file)
	if fileExists(base + ".in") {
		f, _ := os.Open(base + ".in")
		defer f.Close()
		cmd.Stdin = f
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		writeErr(base+".php.error", err)
		return fmt.Errorf("php run error: %w\n%s", err, out)
	}
	phpRes := strings.TrimSpace(strings.ReplaceAll(string(out), "\r\n", "\n"))

	p, err := vm.Compile(prog, env)
	if err != nil {
		writeErr(base+".vm.error", err)
		return fmt.Errorf("vm compile error: %w", err)
	}
	var in io.Reader = os.Stdin
	if fileExists(base + ".in") {
		f, _ := os.Open(base + ".in")
		defer f.Close()
		in = f
	}
	var vmOut bytes.Buffer
	m := vm.NewWithIO(p, in, &vmOut)
	if err := m.Run(); err != nil {
		writeErr(base+".vm.error", err)
		if ve, ok := err.(*vm.VMError); ok {
			return fmt.Errorf("vm run error:\n%s", ve.Format(p))
		}
		return fmt.Errorf("vm run error: %v", err)
	}
	vmRes := strings.TrimSpace(vmOut.String())

	if phpRes != vmRes {
		writeErr(base+".php.error", fmt.Errorf("output mismatch"))
		return fmt.Errorf("output mismatch\n-- php --\n%s\n-- vm --\n%s", phpRes, vmRes)
	}

	if want, err := os.ReadFile(base + ".out"); err == nil {
		if phpRes != strings.TrimSpace(string(want)) {
			writeErr(base+".php.error", fmt.Errorf("golden mismatch"))
			return fmt.Errorf("golden mismatch\n-- got --\n%s\n-- want --\n%s", phpRes, strings.TrimSpace(string(want)))
		}
	}

	os.Remove(base + ".php.error")
	os.Remove(base + ".vm.error")
	return nil
}

func writeErr(path string, err error) {
	os.WriteFile(path, []byte(err.Error()), 0644)
}
