//go:build archived && schemeroundtrip

package schemecode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	schemecode "mochi/archived/x/scheme"
	"mochi/parser"
	"mochi/runtime/vm"
	any2mochi "mochi/tools/any2mochi"
	"mochi/types"
)

// compileAndRun compares the Scheme runtime output with the Mochi VM output.
func compileAndRun(path string) error {
	prog, err := parser.Parse(path)
	if err != nil {
		return fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return fmt.Errorf("type error: %v", errs[0])
	}
	code, err := schemecode.New(env).Compile(prog)
	if err != nil {
		return fmt.Errorf("compile error: %w", err)
	}
	dir, _ := os.MkdirTemp("", "schemert")
	file := filepath.Join(dir, "main.scm")
	if err := os.WriteFile(file, code, 0644); err != nil {
		return fmt.Errorf("write error: %w", err)
	}
	cmd := exec.Command("chibi-scheme", "-m", "chibi", file)
	var inData []byte
	if data, err := os.ReadFile(strings.TrimSuffix(path, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
		inData = data
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("scheme run error: %w\n%s", err, out)
	}
	schemeOut := bytes.TrimSpace(out)

	p, err := vm.Compile(prog, env)
	if err != nil {
		return fmt.Errorf("vm compile error: %w", err)
	}
	var vmBuf bytes.Buffer
	m := vm.NewWithIO(p, bytes.NewReader(inData), &vmBuf)
	if err := m.Run(); err != nil {
		if ve, ok := err.(*vm.VMError); ok {
			return fmt.Errorf("vm run error:\n%s", ve.Format(p))
		}
		return fmt.Errorf("vm run error: %v", err)
	}
	vmOut := bytes.TrimSpace(vmBuf.Bytes())
	if !bytes.Equal(schemeOut, vmOut) {
		return fmt.Errorf("output mismatch")
	}
	return nil
}

func TestScheme_RoundtripVM(t *testing.T) {
	if _, err := schemecode.EnsureScheme(); err != nil {
		t.Skipf("chibi-scheme not installed: %v", err)
	}
	root := any2mochi.FindRepoRoot(t)
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}
	var errs []string
	for _, src := range files {
		name := filepath.Base(src)
		t.Run(name, func(t *testing.T) {
			if err := compileAndRun(src); err != nil {
				errs = append(errs, fmt.Sprintf("%s: %v", name, err))
				t.Log(err)
			}
		})
	}
	any2mochi.WriteErrorsMarkdown(filepath.Join(root, "compile", "x", "scheme"), errs)
}
