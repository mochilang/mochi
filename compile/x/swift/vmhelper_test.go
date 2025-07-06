package swiftcode_test

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	swiftcode "mochi/compile/x/swift"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func compileAndRunSwift(src string) ([]byte, error) {
	prog, err := parser.Parse(src)
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	code, err := swiftcode.New(env).Compile(prog)
	if err != nil {
		return nil, fmt.Errorf("compile error: %w", err)
	}
	dir, err := os.MkdirTemp("", "mochi-swift")
	if err != nil {
		return nil, err
	}
	defer os.RemoveAll(dir)
	file := filepath.Join(dir, "main.swift")
	if err := os.WriteFile(file, code, 0644); err != nil {
		return nil, fmt.Errorf("write error: %w", err)
	}
	exe := filepath.Join(dir, "main")
	if out, err := exec.Command("swiftc", file, "-o", exe).CombinedOutput(); err != nil {
		return nil, fmt.Errorf("swiftc error: %w\n%s", err, out)
	}
	cmd := exec.Command(exe)
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("swift run error: %w\n%s", err, out)
	}
	return bytes.TrimSpace(out), nil
}

func runMochiVM(src string) ([]byte, error) {
	prog, err := parser.Parse(src)
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	p, err := vm.Compile(prog, env)
	if err != nil {
		return nil, fmt.Errorf("compile error: %w", err)
	}
	var in io.Reader = os.Stdin
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		in = bytes.NewReader(data)
	}
	var buf bytes.Buffer
	m := vm.NewWithIO(p, in, &buf)
	if err := m.Run(); err != nil {
		return nil, fmt.Errorf("run error: %w", err)
	}
	return bytes.TrimSpace(buf.Bytes()), nil
}
