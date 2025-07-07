//go:build archived && csroundtrip

package cscode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	cscode "mochi/archived/x/cs"
	"mochi/parser"
	"mochi/runtime/vm"
	converter "mochi/archived/tools/any2mochi/x/cs"
	"mochi/types"
)

func TestCSCompiler_RoundtripVMValid(t *testing.T) {
	if err := cscode.EnsureDotnet(); err != nil {
		t.Skipf("dotnet not installed: %v", err)
	}
	if err := exec.Command("dotnet", "--version").Run(); err != nil {
		t.Skipf("dotnet not runnable: %v", err)
	}

	root := findRepoRoot(t)
	files, err := filepath.Glob(filepath.Join(root, "tests", "vm", "valid", "*.mochi"))
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", filepath.Join(root, "tests", "vm", "valid", "*.mochi"))
	}
	var errs []string
	for _, src := range files {
		name := filepath.Base(src)
		t.Run(name, func(t *testing.T) {
			if err := roundtripRun(src); err != nil {
				t.Error(err)
				errs = append(errs, fmt.Sprintf("%s: %v", name, err))
			}
		})
	}
	writeErrors(filepath.Join(root, "compile", "x", "cs"), errs)
}

func roundtripRun(src string) error {
	prog, err := parser.Parse(src)
	if err != nil {
		return fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return fmt.Errorf("type error: %v", errs[0])
	}
	code, err := cscode.New(env).Compile(prog)
	if err != nil {
		return fmt.Errorf("compile error: %w", err)
	}
	mochiSrc, err := converter.Convert(string(code))
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
	p2, err := vm.CompileWithSource(prog2, env2, string(mochiSrc))
	if err != nil {
		return fmt.Errorf("vm compile error: %w", err)
	}
	var out bytes.Buffer
	m := vm.New(p2, &out)
	if err := m.Run(); err != nil {
		if ve, ok := err.(*vm.VMError); ok {
			return fmt.Errorf("vm run error:\n%s", ve.Format(p2))
		}
		return fmt.Errorf("vm run error: %v", err)
	}
	got := strings.TrimSpace(out.String())
	wantData, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".out")
	if err != nil {
		return fmt.Errorf("missing golden output: %v", err)
	}
	want := strings.TrimSpace(string(wantData))
	if got != want {
		return fmt.Errorf("golden mismatch:\n-- got --\n%s\n-- want --\n%s", got, want)
	}
	return nil
}

func writeErrors(dir string, errs []string) {
	_ = os.MkdirAll(dir, 0755)
	path := filepath.Join(dir, "ERRORS.md")
	var b strings.Builder
	b.WriteString("# Errors\n\n")
	if len(errs) == 0 {
		b.WriteString("None\n")
	} else {
		for _, e := range errs {
			b.WriteString("- " + e + "\n")
		}
	}
	_ = os.WriteFile(path, []byte(b.String()), 0644)
}

func findRepoRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal("cannot determine working directory")
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	t.Fatal("go.mod not found (not in Go module)")
	return ""
}
