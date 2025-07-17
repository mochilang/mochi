//go:build slow

package vm_test

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"testing"

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

// TestVM_JOB_Dataset runs the JOB dataset queries under the VM and compares
// the output with the checked-in golden files under tests/dataset/job/out.
func TestVM_JOB_Dataset(t *testing.T) {
	root := repoRoot(t)
	for i := 1; i <= 33; i++ {
		base := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "job", base+".mochi")
		if _, err := os.Stat(src); err != nil {
			continue
		}
		t.Run(base, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			p, err := vm.Compile(prog, env)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			var out bytes.Buffer
			m := vm.New(p, &out)
			if err := m.Run(); err != nil {
				t.Fatalf("run error: %v", err)
			}
			got := bytes.TrimSpace(out.Bytes())
			wantPath := filepath.Join(root, "tests", "dataset", "job", "out", base+".out")
			want, err := os.ReadFile(wantPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			want = bytes.TrimSpace(want)
			if !bytes.Equal(got, want) {
				t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", base+".out", got, want)
			}
		})
	}
}

// TestVM_JOB_IR_Dataset verifies the disassembled bytecode for each JOB query.
func TestVM_JOB_IR_Dataset(t *testing.T) {
	root := repoRoot(t)
	for i := 1; i <= 33; i++ {
		base := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "job", base+".mochi")
		if _, err := os.Stat(src); err != nil {
			continue
		}
		t.Run(base, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			p, err := vm.Compile(prog, env)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			ir := p.Disassemble(string(data))
			got := []byte(ir)
			wantPath := filepath.Join(root, "tests", "dataset", "job", "out", base+".ir.out")
			want, err := os.ReadFile(wantPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			want = bytes.TrimSpace(want)
			got = bytes.TrimSpace(got)
			if !bytes.Equal(got, want) {
				t.Errorf("IR mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", base+".ir.out", got, want)
			}
		})
	}
}

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
	t.Fatal("go.mod not found (not in Go module)")
	return ""
}
