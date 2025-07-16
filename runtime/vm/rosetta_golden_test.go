//go:build slow

package vm

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
	"mochi/types"
)

func shouldUpdateRosetta() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func writeError(src string, err error) {
	errPath := strings.TrimSuffix(src, filepath.Ext(src)) + ".error"
	_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
}

func removeError(src string) {
	errPath := strings.TrimSuffix(src, filepath.Ext(src)) + ".error"
	os.Remove(errPath)
}

func runVMFile(src string) ([]byte, error) {
	prog, err := parser.Parse(src)
	if err != nil {
		writeError(src, fmt.Errorf("parse error: %w", err))
		return nil, err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeError(src, fmt.Errorf("type error: %v", errs[0]))
		return nil, errs[0]
	}
	p, err := Compile(prog, env)
	if err != nil {
		writeError(src, fmt.Errorf("compile error: %w", err))
		return nil, err
	}
	var out bytes.Buffer
	m := New(p, &out)
	if err := m.Run(); err != nil {
		writeError(src, fmt.Errorf("run error: %w", err))
		return nil, err
	}
	removeError(src)
	return bytes.TrimSpace(out.Bytes()), nil
}

func TestVM_Rosetta_Golden(t *testing.T) {
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests/rosetta/x/Mochi", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	if len(files) == 0 {
		t.Fatal("no Mochi Rosetta tests found")
	}
	max := 5
	if len(files) < max {
		max = len(files)
	}
	for _, src := range files[:max] {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		outPath := filepath.Join(root, "tests/rosetta/x/Mochi", name+".out")
		if _, err := os.Stat(outPath); err != nil {
			continue
		}
		t.Run(name, func(t *testing.T) {
			got, err := runVMFile(src)
			if err != nil {
				t.Skipf("%v", err)
				return
			}
			if shouldUpdateRosetta() {
				if err := os.WriteFile(outPath, append(got, '\n'), 0644); err != nil {
					t.Fatalf("write golden: %v", err)
				}
				t.Logf("updated: %s", outPath)
				return
			}
			want, err := os.ReadFile(outPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(got, bytes.TrimSpace(want)) {
				t.Errorf("%s output\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, got, bytes.TrimSpace(want))
			}
		})
	}
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
