//go:build slow

package tscode_test

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"testing"

	tscode "mochi/compiler/x/ts"
	"mochi/parser"
	"mochi/runtime/mod"
	"mochi/runtime/vm"
	tsconv "mochi/tools/ts2mochi"
	"mochi/types"
)

func TestTSRoundTripVM(t *testing.T) {
	if err := tscode.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}

	root := findRepoRoot2(t)
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	sort.Strings(files)

	var statuses []string
	for _, src := range files {
		name := filepath.Base(src)
		status := runRoundTrip(src)
		statuses = append(statuses, fmt.Sprintf("%s: %s", name, status))
	}

	writeStatusMarkdown(filepath.Join(root, "tests", "any2mochi", "ts"), statuses)
}

func runRoundTrip(path string) string {
	prog, err := parser.Parse(path)
	if err != nil {
		return fmt.Sprintf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return fmt.Sprintf("type error: %v", errs[0])
	}
	modRoot, _ := mod.FindRoot(filepath.Dir(path))
	if modRoot == "" {
		modRoot = filepath.Dir(path)
	}
	tsCode, err := tscode.New(env, modRoot).Compile(prog)
	if err != nil {
		return fmt.Sprintf("compile error: %v", err)
	}
	tmp, err := os.CreateTemp("", "ts-src-*.ts")
	if err != nil {
		return fmt.Sprintf("temp error: %v", err)
	}
	if _, err := tmp.Write(tsCode); err != nil {
		os.Remove(tmp.Name())
		return fmt.Sprintf("temp write: %v", err)
	}
	tmp.Close()
	defer os.Remove(tmp.Name())
	mochiCode, err := tsconv.ConvertFile(tmp.Name())
	if err != nil {
		return fmt.Sprintf("convert error: %v", err)
	}
	prog2, err := parser.ParseString(string(mochiCode))
	if err != nil {
		return fmt.Sprintf("parse2 error: %v", err)
	}
	env2 := types.NewEnv(nil)
	if errs := types.Check(prog2, env2); len(errs) > 0 {
		return fmt.Sprintf("type2 error: %v", errs[0])
	}
	p, err := vm.CompileWithSource(prog2, env2, string(mochiCode))
	if err != nil {
		return fmt.Sprintf("vm compile error: %v", err)
	}
	var buf bytes.Buffer
	m := vm.New(p, &buf)
	if err := m.Run(); err != nil {
		if ve, ok := err.(*vm.VMError); ok {
			return fmt.Sprintf("vm run error: %s", ve.Format(p))
		}
		return fmt.Sprintf("vm run error: %v", err)
	}
	return "ok"
}

func writeStatusMarkdown(dir string, statuses []string) {
	sort.Strings(statuses)
	_ = os.MkdirAll(dir, 0755)
	path := filepath.Join(dir, "ERRORS.md")
	var buf bytes.Buffer
	buf.WriteString("# Round Trip VM Status\n\n")
	for _, s := range statuses {
		buf.WriteString("- " + s + "\n")
	}
	_ = os.WriteFile(path, buf.Bytes(), 0644)
}

func findRepoRoot2(t *testing.T) string {
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
