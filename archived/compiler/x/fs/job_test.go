//go:build archived && slow

package fscode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	fscode "mochi/archived/x/fs"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestFSCompiler_JOB(t *testing.T) {
	if err := fscode.EnsureDotnet(); err != nil {
		t.Skipf("dotnet not installed: %v", err)
	}
	if err := fscode.EnsureFantomas(); err != nil {
		t.Skipf("fantomas not installed: %v", err)
	}
	if home, err := os.UserHomeDir(); err == nil {
		os.Setenv("PATH", os.Getenv("PATH")+":"+filepath.Join(home, ".dotnet", "tools"))
	}

	root := testutil.FindRepoRoot(t)
	for i := 1; i <= 10; i++ {
		q := fmt.Sprintf("q%d", i)
		t.Run(q, func(t *testing.T) {
			src := filepath.Join(root, "tests", "dataset", "job", q+".mochi")
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := fscode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			goldenCode := filepath.Join(root, "tests", "dataset", "job", "compiler", "fs", q+".fs.out")
			if data, err := os.ReadFile(goldenCode); err == nil {
				if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(data)) {
					t.Errorf("generated code mismatch for %s.fs.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(data))
				}
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.fsx")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("dotnet", "fsi", "--quiet", file)
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("fsi error: %v\n%s", err, out)
			}
			goldenOut := filepath.Join(root, "tests", "dataset", "job", "compiler", "fs", q+".out")
			if want, err := os.ReadFile(goldenOut); err == nil {
				if got := strings.TrimSpace(string(out)); got != strings.TrimSpace(string(want)) {
					t.Errorf("unexpected runtime output\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, strings.TrimSpace(string(want)))
				}
			}
		})
	}
}
