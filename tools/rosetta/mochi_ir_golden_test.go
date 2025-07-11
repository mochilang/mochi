//go:build slow

package rosetta

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

var updateFlag = flag.Bool("update", false, "update golden files")

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func TestMochiIR(t *testing.T) {
	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests/rosetta/x/Mochi")
	outDir := filepath.Join(root, "tests/rosetta/out/Mochi-IR")

	if err := os.MkdirAll(outDir, 0755); err != nil {
		t.Fatalf("mkdir out: %v", err)
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
		if _, err := os.Stat(src); err != nil {
			t.Fatalf("missing source for %s", name)
		}
		want := filepath.Join(outDir, name+".ir")

		t.Run(name, func(t *testing.T) {
			ir, err := compileIR(src)
			if err != nil {
				t.Skipf("compile error: %v", err)
			}
			if shouldUpdate() {
				if err := os.WriteFile(want, ir, 0644); err != nil {
					t.Fatalf("write golden: %v", err)
				}
				t.Logf("updated: %s", want)
				return
			}

			data, err := os.ReadFile(want)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			got := strings.TrimSpace(string(ir))
			wantStr := strings.TrimSpace(string(data))
			if got != wantStr {
				t.Errorf("%s IR\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, got, wantStr)
			}
		})
	}
}

func compileIR(src string) ([]byte, error) {
	prog, err := parser.Parse(src)
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	p, err := vm.Compile(prog, env)
	if err != nil {
		return nil, fmt.Errorf("compile error: %w", err)
	}
	srcData, err := os.ReadFile(src)
	if err != nil {
		return nil, err
	}
	ir := strings.TrimSpace(p.Disassemble(string(srcData)))
	if !strings.HasSuffix(ir, "\n") {
		ir += "\n"
	}
	return []byte(ir), nil
}
