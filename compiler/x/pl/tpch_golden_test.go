//go:build slow

package pl_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	pl "mochi/compiler/x/pl"
	"mochi/parser"
	"mochi/types"
)

func repoRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
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
	t.Fatal("repo root not found")
	return ""
}

func TestPrologCompiler_TPCHQueries(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRoot(t)
	for i := 1; i <= 2; i++ {
		base := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "tpc-h", base+".mochi")
		codeWant := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "pl", base+".pl.out")
		outWant := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "pl", base+".out")
		if _, err := os.Stat(codeWant); err != nil {
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
			code, err := pl.New().Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			wantCode, err := os.ReadFile(codeWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.pl.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, got, bytes.TrimSpace(wantCode))
			}

			dir := t.TempDir()
			file := filepath.Join(dir, "main.pl")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("swipl", "-q", file)
			var out bytes.Buffer
			cmd.Stdout = &out
			cmd.Stderr = &out
			if err := cmd.Run(); err != nil {
				t.Fatalf("swipl run error: %v\n%s", err, out.Bytes())
			}
			gotOut := bytes.TrimSpace(out.Bytes())
			wantOut, err := os.ReadFile(outWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}
