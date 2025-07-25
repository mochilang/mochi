//go:build slow

package ex_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/parser"
	ex "mochi/transpiler/x/ex"
	"mochi/types"
)

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
	t.Fatal("go.mod not found")
	return ""
}

func TestElixirTranspiler_VMValid_Golden(t *testing.T) {
	if _, err := exec.LookPath("elixir"); err != nil {
		t.Skip("elixir not installed")
	}
	root := findRepoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ex")
	os.MkdirAll(outDir, 0o755)

	files, _ := filepath.Glob(filepath.Join(outDir, "*.out"))
	var cases []string
	for _, f := range files {
		base := strings.TrimSuffix(filepath.Base(f), ".out")
		cases = append(cases, filepath.Join(root, "tests", "vm", "valid", base+".mochi"))
	}

	for _, src := range cases {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(base, func(t *testing.T) {
			codePath := filepath.Join(outDir, base+".exs")
			outPath := filepath.Join(outDir, base+".out")
			errPath := filepath.Join(outDir, base+".error")

			prog, err := parser.Parse(src)
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Fatalf("parse: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				_ = os.WriteFile(errPath, []byte(errs[0].Error()), 0o644)
				t.Fatalf("type: %v", errs[0])
			}
			gprog, err := ex.Transpile(prog, env)
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Fatalf("transpile: %v", err)
			}
			bench := os.Getenv("MOCHI_BENCHMARK") == "true"
			code := ex.Emit(gprog, bench)
			if err := os.WriteFile(codePath, code, 0o644); err != nil {
				t.Fatalf("write: %v", err)
			}
			cmd := exec.Command("elixir", codePath)
			cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
			out, err := cmd.CombinedOutput()
			if err != nil {
				_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
				t.Fatalf("run: %v", err)
			}
			outBytes := bytes.TrimSpace(out)
			_ = os.WriteFile(outPath, outBytes, 0o644)
			_ = os.Remove(errPath)

			want, _ := os.ReadFile(outPath)
			want = bytes.TrimSpace(want)
			if !bytes.Equal(outBytes, want) {
				t.Fatalf("output mismatch\nGot: %s\nWant: %s", outBytes, want)
			}
		})
	}
}
