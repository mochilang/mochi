//go:build slow

package rbcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	rbcode "mochi/compiler/x/rb"
	"mochi/parser"
	"mochi/types"
)

func stripHeader(b []byte) []byte {
	if bytes.HasPrefix(b, []byte("# Generated")) {
		if i := bytes.IndexByte(b, '\n'); i >= 0 {
			return bytes.TrimSpace(b[i+1:])
		}
	}
	return bytes.TrimSpace(b)
}

func TestRBCompiler_TPCDSQueries(t *testing.T) {
	if err := rbcode.EnsureRuby(); err != nil {
		t.Skipf("ruby not installed: %v", err)
	}
	root := findRepoRoot(t)
	for i := 1; i <= 99; i++ {
		base := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "tpc-ds", base+".mochi")
		codeWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "rb", base+".rb")
		outWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "rb", base+".out")
		errFile := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "rb", base+".error")
		if _, err := os.Stat(codeWant); err != nil {
			continue
		}
		if _, err := os.Stat(outWant); err != nil {
			continue
		}
		t.Run(base, func(t *testing.T) {
			scriptCmd := exec.Command("go", "run", "-tags=slow,archive", "./scripts/compile_tpcds_rb.go")
			scriptCmd.Env = append(os.Environ(), "QUERIES="+fmt.Sprint(i))
			scriptCmd.Dir = root
			if out, err := scriptCmd.CombinedOutput(); err != nil {
				t.Fatalf("compile script error: %v\n%s", err, out)
			}
			if b, err := os.ReadFile(errFile); err == nil {
				t.Fatalf("ruby run failed:\n%s", b)
			}

			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := rbcode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			wantCode, err := os.ReadFile(codeWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			got := stripHeader(code)
			want := stripHeader(wantCode)
			if !bytes.Equal(got, want) {
				t.Errorf("generated code mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base+".rb", got, want)
			}

			dir := t.TempDir()
			file := filepath.Join(dir, "main.rb")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("ruby", file)
			cmd.Dir = root
			var buf bytes.Buffer
			cmd.Stdout = &buf
			cmd.Stderr = &buf
			if err := cmd.Run(); err != nil {
				t.Fatalf("ruby run error: %v\n%s", err, buf.Bytes())
			}
			gotOut := bytes.TrimSpace(buf.Bytes())
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
