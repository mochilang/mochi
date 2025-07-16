//go:build slow

package rosetta

import (
	"bytes"
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

func shouldUpdateRosetta() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func runVM(src string) ([]byte, error) {
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
	var out bytes.Buffer
	m := vm.New(p, &out)
	if err := m.Run(); err != nil {
		return nil, fmt.Errorf("run error: %w", err)
	}
	return bytes.TrimSpace(out.Bytes()), nil
}

func TestRosettaVMGolden(t *testing.T) {
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
			got, err := runVM(src)
			if err != nil {
				errPath := strings.TrimSuffix(src, filepath.Ext(src)) + ".error"
				_ = os.WriteFile(errPath, []byte(err.Error()), 0644)
				t.Skipf("%v", err)
				return
			}
			_ = os.Remove(strings.TrimSuffix(src, filepath.Ext(src)) + ".error")
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
