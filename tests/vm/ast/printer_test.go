//go:build slow

package vm_test

import (
	"bytes"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	"mochi/ast"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func repoRoot(t *testing.T) string {
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
	t.Fatal("go.mod not found")
	return ""
}

func runMochi(src string) ([]byte, error) {
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, errs[0]
	}
	p, err := vm.Compile(prog, env)
	if err != nil {
		return nil, err
	}
	var out bytes.Buffer
	m := vm.New(p, &out)
	if err := m.Run(); err != nil {
		return nil, err
	}
	return bytes.TrimSpace(out.Bytes()), nil
}

func TestASTPrinterGolden(t *testing.T) {
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "vm", "ast")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatal(err)
	}
	sort.Strings(files)

	skip := map[string]bool{"bench_block": true, "tree_sum": true}

	for _, path := range files {
		name := strings.TrimSuffix(filepath.Base(path), ".mochi")
		if skip[name] {
			t.Logf("skip %s", name)
			continue
		}
		prog, err := parser.Parse(path)
		if err != nil {
			t.Fatalf("parse %s: %v", name, err)
		}
		n := ast.FromProgram(prog)
		astPath := filepath.Join(outDir, name+".ast")
		if err := os.WriteFile(astPath, []byte(n.String()), 0o644); err != nil {
			t.Fatal(err)
		}
		var buf bytes.Buffer
		if err := ast.Fprint(&buf, n); err != nil {
			t.Fatalf("print %s: %v", name, err)
		}
		code := buf.String()
		mochiPath := filepath.Join(outDir, name+".mochi")
		if err := os.WriteFile(mochiPath, []byte(code), 0o644); err != nil {
			t.Fatal(err)
		}
		got, err := runMochi(code)
		if err != nil {
			t.Fatalf("run printed %s: %v", name, err)
		}
		want, err := os.ReadFile(filepath.Join(srcDir, name+".out"))
		if err != nil {
			t.Fatalf("missing output for %s", name)
		}
		want = bytes.TrimSpace(want)
		if !bytes.Equal(got, want) {
			t.Errorf("%s output mismatch\nGot: %s\nWant: %s", name, got, want)
		}
	}
}
