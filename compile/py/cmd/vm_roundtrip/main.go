package main

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	pycode "mochi/compile/py"
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
	report.WriteString("# Python roundtrip VM test failures\n\n")
	for _, src := range files {
		if err := process(src); err != nil {
			report.WriteString(fmt.Sprintf("## %s\n\n```\n%s\n```\n\n", src, err))
		}
	}
	if report.Len() == 0 {
		report.WriteString("All Python roundtrip VM tests passed.\n")
	}
	if err := os.WriteFile("compile/py/ERRORS.md", []byte(report.String()), 0644); err != nil {
		fmt.Fprintln(os.Stderr, "write error:", err)
		os.Exit(1)
	}
}

func process(src string) error {
	base := strings.TrimSuffix(src, ".mochi")
	pyPath := base + ".py.out"

	prog, err := parser.Parse(src)
	if err != nil {
		writeErr(base+".py.error", err)
		return fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeErr(base+".py.error", errs[0])
		return fmt.Errorf("type error: %v", errs[0])
	}
	code, err := pycode.New(env).Compile(prog)
	if err != nil {
		writeErr(base+".py.error", err)
		return fmt.Errorf("compile error: %w", err)
	}
	os.WriteFile(pyPath, code, 0644)

	// Run generated Python code
	tmpDir, _ := os.MkdirTemp("", "pyrt")
	defer os.RemoveAll(tmpDir)
	pyFile := filepath.Join(tmpDir, "main.py")
	os.WriteFile(pyFile, code, 0644)
	cmd := exec.Command("python3", pyFile)
	var in io.Reader = os.Stdin
	if fileExists(base + ".in") {
		f, err := os.Open(base + ".in")
		if err == nil {
			defer f.Close()
			in = f
		}
	}
	cmd.Stdin = in
	pyOut, err := cmd.CombinedOutput()
	if err != nil {
		writeErr(base+".py.error", fmt.Errorf("python run error: %w\n%s", err, pyOut))
		return fmt.Errorf("python run error: %w\n%s", err, pyOut)
	}
	pyResult := strings.TrimSpace(string(pyOut))

	// Run program via VM
	p, err := vm.Compile(prog, env)
	if err != nil {
		writeErr(base+".vm.error", err)
		return fmt.Errorf("vm compile error: %w", err)
	}
	var vmOut bytes.Buffer
	vmIn := in
	if in == os.Stdin {
		if fileExists(base + ".in") {
			f, _ := os.Open(base + ".in")
			defer f.Close()
			vmIn = f
		} else {
			vmIn = bytes.NewReader(nil)
		}
	}
	m := vm.NewWithIO(p, vmIn, &vmOut)
	if err := m.Run(); err != nil {
		writeErr(base+".vm.error", err)
		return fmt.Errorf("vm run error: %w", err)
	}
	vmResult := strings.TrimSpace(vmOut.String())

	if pyResult != vmResult {
		writeErr(base+".vm.error", fmt.Errorf("output mismatch\n-- python --\n%s\n-- vm --\n%s", pyResult, vmResult))
		return fmt.Errorf("output mismatch\n-- python --\n%s\n-- vm --\n%s", pyResult, vmResult)
	}

	os.Remove(base + ".py.error")
	os.Remove(base + ".vm.error")
	return nil
}

func writeErr(path string, err error) {
	os.WriteFile(path, []byte(err.Error()), 0644)
}
