//go:build go_vm

package golang

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	gocode "mochi/compile/go"
	"mochi/parser"
	vm "mochi/runtime/vm"
	"mochi/types"
)

// convertRoundTrip compiles the given Mochi file to Go, then converts
// the generated Go code back to Mochi source using the Go any2mochi converter.
func convertRoundTrip(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	prog, err := parser.ParseString(string(data))
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	goSrc, err := gocode.New(env).Compile(prog)
	if err != nil {
		return nil, fmt.Errorf("go compile error: %w", err)
	}
	out, err := Convert(string(goSrc))
	if err != nil {
		return nil, fmt.Errorf("go convert error: %w", err)
	}
	return out, nil
}

// compileAndRun parses, type-checks and executes Mochi source using the VM.
func compileAndRun(src []byte) error {
	prog, err := parser.ParseString(string(src))
	if err != nil {
		return fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return fmt.Errorf("type error: %v", errs[0])
	}
	p, err := vm.CompileWithSource(prog, env, string(src))
	if err != nil {
		return fmt.Errorf("vm compile error: %w", err)
	}
	var buf bytes.Buffer
	m := vm.New(p, &buf)
	if err := m.Run(); err != nil {
		if ve, ok := err.(*vm.VMError); ok {
			return fmt.Errorf("vm run error:\n%s", ve.Format(p))
		}
		return fmt.Errorf("vm run error: %v", err)
	}
	return nil
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

func writeStatusMarkdown(dir string, status map[string]string) {
	_ = os.MkdirAll(dir, 0755)
	path := filepath.Join(dir, "ERRORS.md")
	var sb strings.Builder
	sb.WriteString("# Errors\n\n")
	names := make([]string, 0, len(status))
	for n := range status {
		names = append(names, n)
	}
	sort.Strings(names)
	for _, n := range names {
		if msg := status[n]; msg != "" {
			sb.WriteString("- " + n + ": " + msg + "\n")
		} else {
			sb.WriteString("- " + n + ": ok\n")
		}
	}
	_ = os.WriteFile(path, []byte(sb.String()), 0644)
}

func TestGoRoundTripVM(t *testing.T) {
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests/vm/valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}
	status := make(map[string]string)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		out, err := convertRoundTrip(src)
		if err == nil {
			err = compileAndRun(out)
		}
		if err != nil {
			status[name] = err.Error()
		} else {
			status[name] = ""
		}
	}
	writeStatusMarkdown(filepath.Join(root, "tests/any2mochi/go_vm"), status)
}
