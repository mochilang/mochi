//go:build slow

package pycode_test

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	pycode "mochi/compile/py"
	"mochi/parser"
	"mochi/runtime/vm"
	pyconv "mochi/tools/any2mochi/py"
	"mochi/types"
)

func TestPyRoundTripVM(t *testing.T) {
	if err := pycode.EnsurePython(); err != nil {
		t.Skipf("python3 not installed: %v", err)
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

	writeStatusMarkdown(filepath.Join(root, "compile", "py"), statuses)
}

func runRoundTrip(path string) (res string) {
	defer func() {
		if r := recover(); r != nil {
			res = fmt.Sprintf("panic: %v", r)
		}
	}()
	prog, err := parser.Parse(path)
	if err != nil {
		return fmt.Sprintf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return fmt.Sprintf("type error: %v", errs[0])
	}
	pyCode, err := pycode.New(env).Compile(prog)
	if err != nil {
		return fmt.Sprintf("compile error: %v", err)
	}
	tmp, err := os.CreateTemp("", "py-src-*.py")
	if err != nil {
		return fmt.Sprintf("temp error: %v", err)
	}
	if _, err := tmp.Write(pyCode); err != nil {
		os.Remove(tmp.Name())
		return fmt.Sprintf("temp write: %v", err)
	}
	tmp.Close()
	defer os.Remove(tmp.Name())
	mochiCode, err := pyconv.ConvertFile(tmp.Name())
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
	want, err := os.ReadFile(strings.TrimSuffix(path, ".mochi") + ".out")
	if err == nil {
		got := strings.TrimSpace(buf.String())
		if got != strings.TrimSpace(string(want)) {
			return fmt.Sprintf("golden mismatch:\n-- got --\n%s\n-- want --\n%s", got, strings.TrimSpace(string(want)))
		}
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
