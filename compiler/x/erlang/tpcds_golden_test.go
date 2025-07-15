//go:build slow

package erlang_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	erlang "mochi/compiler/x/erlang"
	"mochi/parser"
	"mochi/types"
)

func TestErlangCompiler_TPCDSQueries(t *testing.T) {
	if _, err := exec.LookPath("escript"); err != nil {
		t.Skip("escript not installed")
	}
	os.Setenv("SOURCE_DATE_EPOCH", "1577977445")
	defer os.Unsetenv("SOURCE_DATE_EPOCH")
	root := repoRoot(t)
	for i := 1; i <= 99; i++ {
		base := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "tpc-ds", base+".mochi")
		codePath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "erlang", base+".erl")
		outPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "erlang", base+".out")
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
			} else {
				wantCode, err := os.ReadFile(codePath)
				if err != nil {
					t.Fatalf("read golden: %v", err)
				}
				got := stripHeader(bytes.TrimSpace(code))
				want := stripHeader(bytes.TrimSpace(wantCode))
				if !bytes.Equal(got, want) {
					t.Errorf("generated code mismatch for %s.erl\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, got, want)
				}
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
			lines := bytes.Split(out, []byte("\n"))
			var buf bytes.Buffer
			for _, line := range lines {
				if bytes.HasPrefix(line, []byte("/tmp/")) {
					continue
				}
				if len(line) > 0 {
					buf.Write(line)
					buf.WriteByte('\n')
				}
			}
			gotOut := bytes.TrimSpace(buf.Bytes())
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
