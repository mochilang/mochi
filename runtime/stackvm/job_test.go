package stackvm

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/parser"
)

func TestStackVM_JOB(t *testing.T) {
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests/dataset/job", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no job source files: %s", pattern)
	}
	found := false
	for _, src := range files {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		want := filepath.Join(root, "tests/dataset/job/out", base+".out")
		irWant := filepath.Join(root, "tests/dataset/job/out", base+".stack.ir.out")
		if _, err := os.Stat(irWant); err != nil {
			continue
		}
		if _, err := os.Stat(want); err != nil {
			continue
		}
		found = true
		name := filepath.Base(src)
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			p, err := Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			var out bytes.Buffer
			v := New(p, &out)
			if err := v.Run(); err != nil {
				t.Fatalf("run error: %v", err)
			}
			got := strings.TrimSpace(out.String())
			data, err := os.ReadFile(want)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			wantStr := strings.TrimSpace(string(data))
			if got != wantStr {
				t.Errorf("%s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, got, wantStr)
			}

			if irWant != "" {
				srcData, err := os.ReadFile(src)
				if err != nil {
					t.Fatalf("read src: %v", err)
				}
				irGot := strings.TrimSpace(p.Disassemble(string(srcData)))
				irData, err := os.ReadFile(irWant)
				if err != nil {
					t.Fatalf("read ir golden: %v", err)
				}
				irWantStr := strings.TrimSpace(string(irData))
				if irGot != irWantStr {
					t.Errorf("%s IR\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, irGot, irWantStr)
				}
			}
		})
	}
	if !found {
		t.Fatal("no job test files")
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
