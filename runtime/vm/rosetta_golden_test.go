//go:build slow

package vm_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	_ "mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestVM_RosettaTasks(t *testing.T) {
	root := findRepoRoot(t)
	dir := filepath.Join(root, "tests/rosetta/x/Mochi")
	pattern := filepath.Join(dir, "*.mochi")
	if only := os.Getenv("MOCHI_ROSETTA_ONLY"); only != "" {
		pattern = filepath.Join(dir, only+".mochi")
	}

	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	if len(files) == 0 {
		t.Fatalf("no Mochi Rosetta tests found: %s", pattern)
	}

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		out := strings.TrimSuffix(src, ".mochi") + ".out"

		t.Run(name, func(t *testing.T) {
			got, err := runMochi(src)
			if err != nil {
				t.Fatalf("run error: %v", err)
			}
			want, err := os.ReadFile(out)
			if err != nil {
				if shouldUpdate() {
					if err2 := os.WriteFile(out, append(got, '\n'), 0644); err2 == nil {
						t.Logf("updated: %s", out)
						return
					} else {
						t.Fatalf("write golden: %v", err2)
					}
				}
				t.Fatalf("read golden: %v", err)
			}

			got = bytes.TrimSpace(got)
			want = bytes.TrimSpace(want)
			if !bytes.Equal(got, want) {
				if shouldUpdate() {
					if err2 := os.WriteFile(out, append(got, '\n'), 0644); err2 == nil {
						t.Logf("updated: %s", out)
						return
					} else {
						t.Fatalf("write golden: %v", err2)
					}
				}
				t.Errorf("golden mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, got, want)
			}
		})
	}
}

func runMochi(src string) ([]byte, error) {
	prog, err := parser.Parse(src)
	if err != nil {
		writeErr(src, err)
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeErr(src, errs[0])
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	p, err := vm.Compile(prog, env)
	if err != nil {
		writeErr(src, err)
		return nil, fmt.Errorf("compile error: %w", err)
	}
	var out bytes.Buffer
	m := vm.New(p, &out)
	if err := m.Run(); err != nil {
		writeErr(src, err)
		return nil, fmt.Errorf("run error: %w", err)
	}
	removeErr(src)
	b := bytes.TrimSpace(out.Bytes())
	if b == nil {
		b = []byte{}
	}
	return b, nil
}

func writeErr(src string, err error) {
	base := strings.TrimSuffix(src, filepath.Ext(src))
	_ = os.WriteFile(base+".error", []byte(err.Error()), 0644)
}

func removeErr(src string) {
	base := strings.TrimSuffix(src, filepath.Ext(src))
	os.Remove(base + ".error")
	os.Remove(base + ".mochi.error")
	os.Remove(base + ".vm.error")
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

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}
