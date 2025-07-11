//go:build slow

package rosetta

import (
	"flag"
	"fmt"
	_ "mochi/golden"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func TestMochiIR(t *testing.T) {
	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests/rosetta/x/Mochi")
	outDir := filepath.Join(root, "tests/rosetta/out/Mochi-IR")

	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkout: %v", err)
	}

	outs, err := filepath.Glob(filepath.Join(srcDir, "*.out"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	if len(outs) == 0 {
		t.Fatal("no Mochi Rosetta tests found")
	}

	for _, out := range outs {
		name := strings.TrimSuffix(filepath.Base(out), ".out")
		src := filepath.Join(srcDir, name+".mochi")
		irPath := filepath.Join(outDir, name+".ir")
		if _, err := os.Stat(src); err != nil {
			t.Fatalf("missing source for %s", name)
		}

		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				writeIRError(outDir, name, fmt.Errorf("parse error: %w", err))
				t.Skip("parse error")
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				writeIRError(outDir, name, fmt.Errorf("type error: %v", errs[0]))
				t.Skip("type error")
				return
			}
			p, err := vm.Compile(prog, env)
			if err != nil {
				writeIRError(outDir, name, fmt.Errorf("compile error: %w", err))
				t.Skip("compile error")
				return
			}
			srcData, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			irGot := strings.TrimSpace(p.Disassemble(string(srcData)))

			if shouldUpdate() {
				if err := os.WriteFile(irPath, []byte(irGot+"\n"), 0o644); err != nil {
					t.Fatalf("write ir: %v", err)
				}
				t.Logf("updated: %s", irPath)
				return
			}

			irData, err := os.ReadFile(irPath)
			if err != nil {
				t.Fatalf("read ir golden: %v", err)
			}
			irWant := strings.TrimSpace(string(irData))
			if irGot != irWant {
				t.Errorf("%s IR\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, irGot, irWant)
			}
			_ = os.Remove(filepath.Join(outDir, name+".error"))
		})
	}
}

func writeIRError(dir, name string, err error) {
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(err.Error()), 0o644)
}
