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

	pascode "mochi/archived/x/pas"
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
	files, err := filepath.Glob("tests/compiler/valid/*.mochi")
	if err != nil {
		fmt.Fprintln(os.Stderr, "glob error:", err)
		os.Exit(1)
	}

	var report strings.Builder
	report.WriteString("# Pascal roundtrip compiler tests\n\n")
	for _, src := range files {
		if err := process(src); err != nil {
			report.WriteString(fmt.Sprintf("## %s\n\n```\n%s\n```\n\n", src, err))
		}
	}
	if report.Len() == 0 {
		report.WriteString("All Pascal compiler tests passed.\n")
	}
	os.WriteFile("compile/x/pas/ERRORS.md", []byte(report.String()), 0644)
}

func process(src string) error {
	base := strings.TrimSuffix(src, ".mochi")
	pasPath := base + ".pas.out"

	prog, err := parser.Parse(src)
	if err != nil {
		writeErr(base+".pas.error", err)
		return fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeErr(base+".pas.error", errs[0])
		return fmt.Errorf("type error: %v", errs[0])
	}
	code, err := pascode.New(env).Compile(prog)
	if err != nil {
		writeErr(base+".pas.error", err)
		return fmt.Errorf("compile error: %w", err)
	}
	if err := os.WriteFile(pasPath, code, 0644); err != nil {
		return err
	}

	fpc, err := pascode.EnsureFPC()
	if err != nil {
		writeErr(base+".pas.error", err)
		return fmt.Errorf("fpc not installed: %v", err)
	}
	exe := filepath.Join(filepath.Dir(pasPath), filepath.Base(base))
	build := exec.Command(fpc, "-o"+filepath.Base(exe), filepath.Base(pasPath))
	build.Dir = filepath.Dir(pasPath)
	if out, err := build.CombinedOutput(); err != nil {
		writeErr(base+".pas.error", fmt.Errorf("fpc error: %w\n%s", err, out))
		return fmt.Errorf("fpc error: %w\n%s", err, out)
	}

	run := exec.Command(exe)
	if data, err := os.ReadFile(base + ".in"); err == nil {
		run.Stdin = bytes.NewReader(data)
	}
	var runOut bytes.Buffer
	run.Stdout = &runOut
	run.Stderr = &runOut
	if err := run.Run(); err != nil {
		writeErr(base+".pas.error", fmt.Errorf("run error: %w\n%s", err, runOut.Bytes()))
		return fmt.Errorf("run error: %w\n%s", err, runOut.Bytes())
	}
	got := strings.TrimSpace(runOut.String())

	// Run with VM for expected output
	p, err := vm.Compile(prog, env)
	if err != nil {
		writeErr(base+".pas.error", err)
		return fmt.Errorf("vm compile error: %w", err)
	}
	var in io.Reader = nil
	if fileExists(base + ".in") {
		f, err := os.Open(base + ".in")
		if err == nil {
			defer f.Close()
			in = f
		}
	}
	var vmOut bytes.Buffer
	m := vm.NewWithIO(p, in, &vmOut)
	if err := m.Run(); err != nil {
		writeErr(base+".pas.error", err)
		return fmt.Errorf("vm run error: %w", err)
	}
	want := strings.TrimSpace(vmOut.String())

	if got != want {
		writeErr(base+".pas.error", fmt.Errorf("output mismatch\n-- got --\n%s\n-- want --\n%s", got, want))
		return fmt.Errorf("output mismatch\n-- got --\n%s\n-- want --\n%s", got, want)
	}

	// Clean up previous error
	os.Remove(base + ".pas.error")
	return nil
}

func writeErr(path string, err error) {
	os.WriteFile(path, []byte(err.Error()), 0644)
}
