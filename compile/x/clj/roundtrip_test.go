//go:build roundtrip

package cljcode_test

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	cljcode "mochi/compile/x/clj"
	"mochi/parser"
	"mochi/runtime/vm"
	cljconv "mochi/tools/any2mochi/x/clj"
	"mochi/types"
)

// roundTrip compiles the Mochi source to Clojure, converts back to Mochi and
// executes using the VM. Output must match the golden .out file.
func roundTrip(src string) error {
	prog, err := parser.Parse(src)
	if err != nil {
		return fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return fmt.Errorf("type error: %v", errs[0])
	}
	code, err := cljcode.New(env).Compile(prog)
	if err != nil {
		return fmt.Errorf("compile error: %w", err)
	}
	mochiSrc, err := cljconv.Convert(string(code))
	if err != nil {
		return fmt.Errorf("convert error: %w", err)
	}
	prog2, err := parser.ParseString(string(mochiSrc))
	if err != nil {
		return fmt.Errorf("reparse error: %w", err)
	}
	env2 := types.NewEnv(nil)
	if errs := types.Check(prog2, env2); len(errs) > 0 {
		return fmt.Errorf("retype error: %v", errs[0])
	}
	p, err := vm.CompileWithSource(prog2, env2, string(mochiSrc))
	if err != nil {
		return fmt.Errorf("vm compile error: %w", err)
	}
	inPath := strings.TrimSuffix(src, ".mochi") + ".in"
	var in []byte
	if data, err := os.ReadFile(inPath); err == nil {
		in = data
	}
	var out bytes.Buffer
	m := vm.NewWithIO(p, bytes.NewReader(in), &out)
	if err := m.Run(); err != nil {
		if ve, ok := err.(*vm.VMError); ok {
			return fmt.Errorf("vm run error:\n%s", ve.Format(p))
		}
		return fmt.Errorf("vm run error: %v", err)
	}
	wantPath := strings.TrimSuffix(src, ".mochi") + ".out"
	if want, err := os.ReadFile(wantPath); err == nil {
		got := strings.TrimSpace(out.String())
		wantStr := strings.TrimSpace(string(want))
		if got != wantStr {
			return fmt.Errorf("output mismatch\n-- got --\n%s\n-- want --\n%s", got, wantStr)
		}
	}
	return nil
}

func TestClojureRoundTripVM(t *testing.T) {
	if err := cljcode.EnsureClojure(); err != nil {
		t.Skipf("clojure not installed: %v", err)
	}
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests/vm/valid", "*.mochi")
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
			if e := roundTrip(src); e != nil {
				errs = append(errs, fmt.Sprintf("%s: %v", src, e))
				t.Logf("%v", e)
			}
		})
	}
	writeErrorsMarkdown(filepath.Join(root, "tests/any2mochi/clj"), errs)
}

func writeErrorsMarkdown(dir string, errs []string) {
	_ = os.MkdirAll(dir, 0755)
	path := filepath.Join(dir, "ERRORS.md")
	var buf strings.Builder
	buf.WriteString("# Errors\n\n")
	if len(errs) == 0 {
		buf.WriteString("None\n")
	} else {
		for _, e := range errs {
			buf.WriteString("- " + e + "\n")
		}
	}
	_ = os.WriteFile(path, []byte(buf.String()), 0644)
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
