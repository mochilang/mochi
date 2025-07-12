//go:build slow

package fscode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	fscode "mochi/compiler/x/fs"
	"mochi/parser"
	"mochi/types"
)

func TestFSCompiler_TPCH(t *testing.T) {
	if _, err := exec.LookPath("fsharpc"); err != nil {
		t.Skip("fsharpc not installed")
	}
	if _, err := exec.LookPath("mono"); err != nil {
		t.Skip("mono not installed")
	}
	root := repoRoot(t)

	runQuery := func(q string) {
		src := filepath.Join(root, "tests", "dataset", "tpc-h", q+".mochi")
		codeWant := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "fs", q+".fs.out")
		outWant := filepath.Join(root, "tests", "dataset", "tpc-h", "out", q+".out")

		prog, err := parser.Parse(src)
		if err != nil {
			t.Fatalf("parse error: %v", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			t.Fatalf("type error: %v", errs[0])
		}
		code, err := fscode.CompileFile(src)
		if err != nil {
			t.Fatalf("compile error: %v", err)
		}
		wantCode, err := os.ReadFile(codeWant)
		if err != nil {
			t.Fatalf("read golden: %v", err)
		}
		if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
			t.Errorf("generated code mismatch for %s.fs.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(wantCode))
		}
		dir := t.TempDir()
		fsPath := filepath.Join(dir, "main.fs")
		if err := os.WriteFile(fsPath, code, 0644); err != nil {
			t.Fatalf("write error: %v", err)
		}
		exe := filepath.Join(dir, "main.exe")
		cmd := exec.Command("fsharpc", "--target:exe", fmt.Sprintf("--out:%s", exe), fsPath)
		out, err := cmd.CombinedOutput()
		if err != nil {
			t.Fatalf("fsharpc error: %v\n%s", err, out)
		}
		run := exec.Command("mono", exe)
		var stdout bytes.Buffer
		run.Stdout = &stdout
		run.Stderr = &stdout
		if err := run.Run(); err != nil {
			t.Fatalf("mono error: %v\n%s", err, stdout.Bytes())
		}
		gotOut := bytes.TrimSpace(stdout.Bytes())
		wantOut, err := os.ReadFile(outWant)
		if err != nil {
			t.Fatalf("read golden: %v", err)
		}
		if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
			t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotOut, bytes.TrimSpace(wantOut))
		}
	}

	for _, q := range []string{"q1", "q2"} {
		t.Run(q, func(t *testing.T) { runQuery(q) })
	}
}
