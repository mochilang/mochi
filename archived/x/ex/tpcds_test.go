//go:build archived && slow

package excode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	excode "mochi/archived/x/ex"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestExCompiler_TPCDSQueries(t *testing.T) {
	if err := excode.EnsureElixir(); err != nil {
		t.Skipf("elixir not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	for i := 1; i <= 99; i++ {
		q := fmt.Sprintf("q%d", i)
		t.Run(q, func(t *testing.T) {
			src := filepath.Join(root, "tests", "dataset", "tpc-ds", q+".mochi")
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := excode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			codeWantPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "ex", q+".ex.out")
			wantCode, err := os.ReadFile(codeWantPath)
			if err != nil {
				t.Logf("missing golden %s", codeWantPath)
				return
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.ex.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", q, got, bytes.TrimSpace(wantCode))
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.exs")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("elixir", file)
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("elixir run error: %v\n%s", err, out)
			}
			lines := bytes.Split(out, []byte("\n"))
			filtered := make([][]byte, 0, len(lines))
			for _, l := range lines {
				if bytes.HasPrefix(l, []byte("warning:")) || bytes.Contains(l, []byte(".exs:")) {
					continue
				}
				filtered = append(filtered, l)
			}
			gotOut := bytes.TrimSpace(bytes.Join(filtered, []byte("\n")))
			outWantPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "ex", q+".out")
			wantOut, err := os.ReadFile(outWantPath)
			if err != nil {
				t.Logf("missing golden %s", outWantPath)
				return
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", q, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}
