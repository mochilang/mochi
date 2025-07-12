//go:build slow

package typescriptcode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	typescriptcode "mochi/compiler/x/typescript"
	"mochi/parser"
	"mochi/types"
)

func TestTypeScriptCompiler_TPCHQuery1(t *testing.T) {
	if _, err := exec.LookPath("deno"); err != nil {
		t.Skip("deno not installed")
	}
	root := findRepoRoot(t)
	base := "q1"
	src := filepath.Join(root, "tests", "dataset", "tpc-h", base+".mochi")
	codeWant := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "typescript", base+".ts.out")
	outWant := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "typescript", base+".out")

	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := typescriptcode.New().Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	wantCode, err := os.ReadFile(codeWant)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
		t.Errorf("generated code mismatch for %s.ts.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, got, bytes.TrimSpace(wantCode))
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.ts")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("deno", "run", "--quiet", file)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("deno run error: %v\n%s", err, out)
	}
	gotOut := bytes.TrimSpace(out)
	wantOut, err := os.ReadFile(outWant)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
		t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, gotOut, bytes.TrimSpace(wantOut))
	}
}
