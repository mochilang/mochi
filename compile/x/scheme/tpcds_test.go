//go:build slow

package schemecode_test

import (
	"bytes"
	"os"
	"path/filepath"
	"testing"

	schemecode "mochi/compile/x/scheme"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestSchemeCompiler_TPCDSQ1(t *testing.T) {
	if _, err := schemecode.EnsureScheme(); err != nil {
		t.Skipf("scheme not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "tpc-ds", "q1.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := schemecode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	wantCode, err := os.ReadFile(filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "scheme", "q1.scm.out"))
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
		t.Errorf("generated code mismatch for q1.scm.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, bytes.TrimSpace(wantCode))
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.scm")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	t.Skip("Scheme runtime check disabled until missing libraries are available")
}

func TestSchemeCompiler_TPCDSQ2(t *testing.T) {
	if _, err := schemecode.EnsureScheme(); err != nil {
		t.Skipf("scheme not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "tpc-ds", "q2.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := schemecode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	wantCode, err := os.ReadFile(filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "scheme", "q2.scm.out"))
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
		t.Errorf("generated code mismatch for q2.scm.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, bytes.TrimSpace(wantCode))
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.scm")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	t.Skip("Scheme runtime check disabled until missing libraries are available")
}

func TestSchemeCompiler_TPCDSQ3(t *testing.T) {
	if _, err := schemecode.EnsureScheme(); err != nil {
		t.Skipf("scheme not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "tpc-ds", "q3.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := schemecode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	wantCode, err := os.ReadFile(filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "scheme", "q3.scm.out"))
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
		t.Errorf("generated code mismatch for q3.scm.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, bytes.TrimSpace(wantCode))
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.scm")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	t.Skip("Scheme runtime check disabled until missing libraries are available")
}
