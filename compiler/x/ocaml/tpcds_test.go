//go:build slow

package ocaml_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	ocaml "mochi/compiler/x/ocaml"
	testutil "mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestOCamlCompiler_TPCDS(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skipf("ocamlc not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	for i := 1; i <= 99; i++ {
		q := fmt.Sprintf("q%d", i)
		codeWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "ocaml", q+".ml")
		outWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "ocaml", q+".out")
		if _, err := os.Stat(codeWant); err != nil {
			continue
		}
		if _, err := os.Stat(outWant); err != nil {
			continue
		}
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
			code, err := ocaml.New(env).Compile(prog, src)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			wantCode, err := os.ReadFile(codeWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(bytes.TrimSpace(code), bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.ml\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, code, wantCode)
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "prog.ml")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			exe := filepath.Join(dir, "prog")
			if out, err := exec.Command("ocamlc", file, "-o", exe).CombinedOutput(); err != nil {
				t.Fatalf("ocamlc error: %v\n%s", err, out)
			}
			out, err := exec.Command(exe).CombinedOutput()
			if err != nil {
				t.Fatalf("run error: %v\n%s", err, out)
			}
			gotOut := bytes.TrimSpace(out)
			wantOut, err := os.ReadFile(outWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}
