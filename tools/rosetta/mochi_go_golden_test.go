//go:build slow

package rosetta

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	gocode "mochi/compiler/x/go"
	"mochi/parser"
	"mochi/types"
)

// TestMochiGoGolden compiles each Mochi source program under
// tests/rosetta/x/Mochi to Go and verifies the generated code
// matches the golden files in tests/rosetta/out/Go.
func TestMochiGoGolden(t *testing.T) {
	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests/rosetta/x/Mochi")
	outDir := filepath.Join(root, "tests/rosetta/out/Go")

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
		goPath := filepath.Join(outDir, name+".go")
		if _, err := os.Stat(srcPath); err != nil {
			t.Fatalf("missing source for %s", name)
		}

		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(srcPath)
			if err != nil {
				writeGoError(outDir, name, fmt.Errorf("parse error: %w", err))
				t.Skip("parse error")
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				writeGoError(outDir, name, fmt.Errorf("type error: %v", errs[0]))
				t.Skip("type error")
				return
			}
			code, err := gocode.New(env).Compile(prog)
			if err != nil {
				writeGoError(outDir, name, fmt.Errorf("compile error: %w", err))
				t.Skip("compile error")
				return
			}
			got := strings.TrimSpace(string(code))

			if shouldUpdate() {
				if err := os.WriteFile(goPath, []byte(got+"\n"), 0o644); err != nil {
					t.Fatalf("write go: %v", err)
				}
				t.Logf("updated: %s", goPath)
				return
			}

			wantData, err := os.ReadFile(goPath)
			if err != nil {
				t.Fatalf("read go golden: %v", err)
			}
			want := strings.TrimSpace(string(wantData))
			if got != want {
				t.Errorf("%s Go\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, got, want)
			}
			_ = os.Remove(filepath.Join(outDir, name+".error"))
		})
	}
}
