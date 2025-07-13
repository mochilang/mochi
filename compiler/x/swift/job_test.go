package swift_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	swift "mochi/compiler/x/swift"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestSwiftCompiler_JOB_Q1(t *testing.T) {
	swiftExe := ensureSwift(t)
	root := testutil.FindRepoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "job", "q1.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := swift.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	wantCodePath := filepath.Join(root, "tests", "dataset", "job", "compiler", "swift", "q1.swift.out")
	wantCode, err := os.ReadFile(wantCodePath)
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
		t.Errorf("generated code mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s", got, want)
	}
	out, err := compileAndRunSwiftSrc(t, swiftExe, code)
	if err != nil {
		t.Fatalf("swift run error: %v", err)
	}
	wantOutPath := filepath.Join(root, "tests", "dataset", "job", "out", "q1.out")
	wantOut, err := os.ReadFile(wantOutPath)
	if err != nil {
		t.Fatalf("read golden out: %v", err)
	}
	if !bytes.Equal(bytes.TrimSpace(out), bytes.TrimSpace(wantOut)) {
		t.Fatalf("output mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s", out, wantOut)
	}
}

func compileAndRunSwiftSrc(t *testing.T, swiftExe string, code []byte) ([]byte, error) {
	dir := t.TempDir()
	file := filepath.Join(dir, "main.swift")
	if err := os.WriteFile(file, code, 0644); err != nil {
		return nil, err
	}
	exe := filepath.Join(dir, "main")
	if out, err := exec.Command("swiftc", file, "-o", exe).CombinedOutput(); err != nil {
		return out, err
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		return out, err
	}
	return out, nil
}
