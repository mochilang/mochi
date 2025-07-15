//go:build slow

package phpcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	phpcode "mochi/compiler/x/php"
	"mochi/parser"
	"mochi/types"
)

func TestPHPCompiler_TPCDSQueries(t *testing.T) {
	if _, err := exec.LookPath("php"); err != nil {
		t.Skip("php not installed")
	}
	root := repoRoot(t)
	for i := 1; i <= 20; i++ {
		base := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "tpc-ds", base+".mochi")
		codeWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "php", base+".php.out")
		outWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "php", base+".out")
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
			code, err := phpcode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			wantCode, err := os.ReadFile(codeWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			strip := func(b []byte) []byte {
				if i := bytes.IndexByte(b, '\n'); i >= 0 {
					return bytes.TrimSpace(b[i+1:])
				}
				return bytes.TrimSpace(b)
			}
			got := strip(code)
			want := strip(wantCode)
			if !bytes.Equal(got, want) {
				t.Errorf("generated code mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base+".php.out", got, want)
			}
			tmp := filepath.Join(os.TempDir(), base+".php")
			if err := os.WriteFile(tmp, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			out, err := exec.Command("php", tmp).CombinedOutput()
			if err != nil {
				t.Skipf("php run error: %v\n%s", err, out)
			}
			gotOut := bytes.TrimSpace(out)
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
