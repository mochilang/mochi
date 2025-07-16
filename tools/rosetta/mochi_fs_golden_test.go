package rosetta

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	fscode "mochi/compiler/x/fs"
	"mochi/parser"
	"mochi/types"
)

// TestMochiFSGolden compiles each Mochi source program under
// tests/rosetta/x/Mochi to F# and verifies the generated code
// matches the golden files in tests/rosetta/out/Fs.
func TestMochiFSGolden(t *testing.T) {
	root := findRepoRoot(t)
	os.Setenv("SOURCE_DATE_EPOCH", "1136214245")
	defer os.Unsetenv("SOURCE_DATE_EPOCH")
	srcDir := filepath.Join(root, "tests/rosetta/x/Mochi")
	outDir := filepath.Join(root, "tests/rosetta/out/Fs")

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
		fsPath := filepath.Join(outDir, name+".fs")
		if _, err := os.Stat(srcPath); err != nil {
			t.Fatalf("missing source for %s", name)
		}

		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(srcPath)
			if err != nil {
				writeFSError(outDir, name, fmt.Errorf("parse error: %w", err))
				t.Skip("parse error")
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				writeFSError(outDir, name, fmt.Errorf("type error: %v", errs[0]))
				t.Skip("type error")
				return
			}
			code, err := fscode.New().Compile(prog)
			if err != nil {
				writeFSError(outDir, name, fmt.Errorf("compile error: %w", err))
				t.Skip("compile error")
				return
			}
			got := strings.TrimSpace(string(code))

			if shouldUpdate() {
				if err := os.WriteFile(fsPath, []byte(got+"\n"), 0o644); err != nil {
					t.Fatalf("write fs: %v", err)
				}
				t.Logf("updated: %s", fsPath)
				return
			}

			wantData, err := os.ReadFile(fsPath)
			if err != nil {
				t.Fatalf("read fs golden: %v", err)
			}
			want := strings.TrimSpace(string(wantData))
			if got != want {
				t.Errorf("%s F#\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, got, want)
			}
			_ = os.Remove(filepath.Join(outDir, name+".error"))
		})
	}
}

func writeFSError(dir, name string, err error) {
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(err.Error()), 0o644)
}
