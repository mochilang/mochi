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

	scalacode "mochi/compile/x/scala"
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
	if limStr := os.Getenv("LIMIT"); limStr != "" {
		if lim, err := strconv.Atoi(limStr); err == nil && lim < len(files) {
			files = files[:lim]
		}
	}

	var report strings.Builder
	report.WriteString("# Scala VM test failures\n\n")
	for _, src := range files {
		if err := runFile(src); err != nil {
			report.WriteString(fmt.Sprintf("## %s\n\n```\n%s\n```\n\n", src, err))
		}
	}
	if report.Len() == 0 {
		report.WriteString("All Scala VM tests passed.\n")
	}
	if err := os.WriteFile("compile/x/scala/ERRORS.md", []byte(report.String()), 0644); err != nil {
		fmt.Fprintln(os.Stderr, "write error:", err)
		os.Exit(1)
	}
}

func runFile(src string) error {
	base := strings.TrimSuffix(src, ".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		return fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return fmt.Errorf("type error: %v", errs[0])
	}
	code, err := scalacode.New(env).Compile(prog)
	if err != nil {
		return fmt.Errorf("compile error: %w", err)
	}
	dir, err := os.MkdirTemp("", "scala-src-*")
	if err != nil {
		return fmt.Errorf("temp dir: %w", err)
	}
	file := filepath.Join(dir, "Main.scala")
	if err := os.WriteFile(file, code, 0644); err != nil {
		return fmt.Errorf("write scala: %w", err)
	}
	if out, err := exec.Command("scalac", file).CombinedOutput(); err != nil {
		return fmt.Errorf("scalac error: %w\n%s", err, out)
	}
	scalaCmd := "scala"
	args := []string{"Main"}
	if _, err := exec.LookPath("scala-cli"); err == nil {
		scalaCmd = "scala-cli"
		args = []string{"run", file}
	} else if out, err := exec.Command("scala", "-version").CombinedOutput(); err == nil && bytes.Contains(out, []byte("Scala CLI")) {
		args = []string{"run", file}
	}
	cmd := exec.Command(scalaCmd, args...)
	if fileExists(base + ".in") {
		f, err := os.Open(base + ".in")
		if err == nil {
			defer f.Close()
			cmd.Stdin = f
		}
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("scala run error: %w\n%s", err, out)
	}
	scalaOut := strings.TrimSpace(string(out))

	vmOut, err := runVM(prog, env, base)
	if err != nil {
		return err
	}
	if scalaOut != vmOut {
		return fmt.Errorf("output mismatch\n-- scala --\n%s\n-- vm --\n%s", scalaOut, vmOut)
	}
	return nil
}

func runVM(prog *parser.Program, env *types.Env, base string) (string, error) {
	p, err := vm.Compile(prog, env)
	if err != nil {
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
			return "", fmt.Errorf("vm run error:\n%s", ve.Format(p))
		}
		return "", fmt.Errorf("vm run error: %v", err)
	}
	return strings.TrimSpace(buf.String()), nil
}
