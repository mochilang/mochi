package main

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	rktcode "mochi/compile/x/rkt"
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
	pattern := "tests/vm/valid/*.mochi"
	if len(os.Args) > 1 {
		pattern = os.Args[1]
	}
	files, err := filepath.Glob(pattern)
	if err != nil {
		fmt.Fprintln(os.Stderr, "glob error:", err)
		os.Exit(1)
	}

	var report strings.Builder
	report.WriteString("# Racket compiler VM comparison failures\n\n")
	hadErr := false
	for _, src := range files {
		if err := process(src); err != nil {
			hadErr = true
			report.WriteString(fmt.Sprintf("## %s\n\n```\n%s\n```\n\n", src, err))
		}
	}
	if !hadErr {
		report.WriteString("All Racket compiler tests passed when compared with VM.\n")
	}
	os.WriteFile("compile/x/rkt/ERRORS.md", []byte(report.String()), 0644)
}

func process(src string) error {
	base := strings.TrimSuffix(src, ".mochi")
	rktPath := base + ".rkt.out"

	prog, err := parser.Parse(src)
	if err != nil {
		writeErr(base+".rkt.error", err)
		return fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeErr(base+".rkt.error", errs[0])
		return fmt.Errorf("type error: %v", errs[0])
	}
	code, err := rktcode.New(env).Compile(prog)
	if err != nil {
		writeErr(base+".rkt.error", err)
		return fmt.Errorf("compile error: %w", err)
	}
	os.WriteFile(rktPath, code, 0644)

	tmpDir, err := os.MkdirTemp("", "rkt")
	if err != nil {
		writeErr(base+".rkt.error", err)
		return err
	}
	defer os.RemoveAll(tmpDir)
	tmpFile := filepath.Join(tmpDir, "main.rkt")
	if err := os.WriteFile(tmpFile, code, 0644); err != nil {
		writeErr(base+".rkt.error", err)
		return fmt.Errorf("write temp: %w", err)
	}
	cmd := exec.Command("racket", tmpFile)
	if fileExists(base + ".in") {
		f, err := os.Open(base + ".in")
		if err == nil {
			defer f.Close()
			cmd.Stdin = f
		}
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		writeErr(base+".rkt.error", fmt.Errorf("racket run error: %w\n%s", err, out))
		return fmt.Errorf("racket run error: %w\n%s", err, out)
	}
	racketOut := strings.TrimSpace(string(out))

	vmOut, err := runVM(prog, env, base)
	if err != nil {
		return err
	}

	if racketOut != vmOut {
		writeErr(base+".vm.error", fmt.Errorf("output mismatch\n-- racket --\n%s\n-- vm --\n%s", racketOut, vmOut))
		return fmt.Errorf("output mismatch\n-- racket --\n%s\n-- vm --\n%s", racketOut, vmOut)
	}

	// compare with golden .out if exists
	if want, err := os.ReadFile(base + ".out"); err == nil {
		if racketOut != strings.TrimSpace(string(want)) {
			writeErr(base+".rkt.error", fmt.Errorf("golden mismatch\n-- got --\n%s\n-- want --\n%s", racketOut, strings.TrimSpace(string(want))))
			return fmt.Errorf("golden mismatch\n-- got --\n%s\n-- want --\n%s", racketOut, strings.TrimSpace(string(want)))
		}
	}

	os.Remove(base + ".rkt.error")
	os.Remove(base + ".vm.error")
	return nil
}

func runVM(prog *parser.Program, env *types.Env, base string) (string, error) {
	p, err := vm.Compile(prog, env)
	if err != nil {
		writeErr(base+".vm.error", err)
		return "", fmt.Errorf("vm compile error: %w", err)
	}
	var in io.Reader = os.Stdin
	if fileExists(base + ".in") {
		f, err := os.Open(base + ".in")
		if err == nil {
			defer f.Close()
			in = f
		}
	}
	var buf bytes.Buffer
	m := vm.NewWithIO(p, in, &buf)
	if err := m.Run(); err != nil {
		if ve, ok := err.(*vm.VMError); ok {
			writeErr(base+".vm.error", fmt.Errorf("vm run error:\n%s", ve.Format(p)))
			return "", fmt.Errorf("vm run error:\n%s", ve.Format(p))
		}
		writeErr(base+".vm.error", err)
		return "", fmt.Errorf("vm run error: %w", err)
	}
	return strings.TrimSpace(buf.String()), nil
}

func writeErr(path string, err error) {
	os.WriteFile(path, []byte(err.Error()), 0644)
}
