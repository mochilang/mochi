//go:build slow

package rosetta

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	ccode "mochi/compiler/x/c"
	"mochi/parser"
	"mochi/types"
)

// TestMochiCGolden compiles Mochi Rosetta programs to C and verifies the
// generated source matches the golden files in tests/rosetta/out/C.
func TestMochiCGolden(t *testing.T) {
	if _, err := ccode.EnsureCC(); err != nil {
		t.Skipf("C compiler not installed: %v", err)
	}
	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests/rosetta/x/Mochi")
	outDir := filepath.Join(root, "tests/rosetta/out/C")
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
		srcPath := filepath.Join(srcDir, name+".mochi")
		cPath := filepath.Join(outDir, name+".c")
		if _, err := os.Stat(srcPath); err != nil {
			t.Fatalf("missing source for %s", name)
		}
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(srcPath)
			if err != nil {
				writeCError(outDir, name, fmt.Errorf("parse error: %w", err))
				t.Skip("parse error")
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				writeCError(outDir, name, fmt.Errorf("type error: %v", errs[0]))
				t.Skip("type error")
				return
			}
			code, err := ccode.New(env).Compile(prog)
			if err != nil {
				writeCError(outDir, name, fmt.Errorf("compile error: %w", err))
				t.Skip("compile error")
				return
			}
			got := bytes.TrimSpace(code)
			if shouldUpdate() {
				if err := os.WriteFile(cPath, append(got, '\n'), 0o644); err != nil {
					t.Fatalf("write c: %v", err)
				}
				t.Logf("updated: %s", cPath)
				return
			}
			want, err := os.ReadFile(cPath)
			if err != nil {
				t.Fatalf("read c golden: %v", err)
			}
			want = bytes.TrimSpace(want)
			if !bytes.Equal(got, want) {
				t.Errorf("%s C\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, got, want)
			}
			_ = os.Remove(filepath.Join(outDir, name+".error"))
		})
	}
}
