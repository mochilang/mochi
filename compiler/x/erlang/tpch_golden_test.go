//go:build slow

package erlang_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	erlang "mochi/compiler/x/erlang"
	"mochi/parser"
	"mochi/types"
)

var update = flag.Bool("update", false, "update golden files")

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func TestErlangCompiler_TPCHQueries(t *testing.T) {
	if _, err := exec.LookPath("escript"); err != nil {
		t.Skip("escript not installed")
	}
	os.Setenv("SOURCE_DATE_EPOCH", "1577977445")
	defer os.Unsetenv("SOURCE_DATE_EPOCH")
	root := repoRoot(t)
	for i := 1; i <= 22; i++ {
		base := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "tpc-h", base+".mochi")
		codePath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "erlang", base+".erl")
		outPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "erlang", base+".out")
		if !shouldUpdate() {
			if _, err := os.Stat(outPath); err != nil {
				continue
			}
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
			code, err := erlang.New(src).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			if shouldUpdate() {
				_ = os.WriteFile(codePath, code, 0644)
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.erl")
			if err := os.WriteFile(file, code, 0755); err != nil {
				t.Fatalf("write error: %v", err)
			}
			out, err := exec.Command("escript", file).CombinedOutput()
			if err != nil {
				t.Fatalf("escript error: %v\n%s", err, out)
			}
			gotOut := bytes.TrimSpace(out)
			if shouldUpdate() {
				_ = os.WriteFile(outPath, append(gotOut, '\n'), 0644)
			} else {
				wantOut, err := os.ReadFile(outPath)
				if err != nil {
					t.Fatalf("read golden: %v", err)
				}
				if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
					t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, gotOut, bytes.TrimSpace(wantOut))
				}
			}
		})
	}
}

func repoRoot(t *testing.T) string {
	t.Helper()
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
