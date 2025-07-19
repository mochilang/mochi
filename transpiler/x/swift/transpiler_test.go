//go:build slow

package swifttrans_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"mochi/compiler/x/testutil"
	"mochi/parser"
	swifttrans "mochi/transpiler/x/swift"
	"mochi/types"
)

func ensureSwift(t *testing.T) string {
	if env := os.Getenv("SWIFT"); env != "" {
		if p, err := exec.LookPath(env); err == nil {
			return p
		}
	}
	if p, err := exec.LookPath("swiftc"); err == nil {
		return p
	}
	if p, err := exec.LookPath("swift"); err == nil {
		return p
	}
	t.Skip("swift not found")
	return ""
}

func compileAndRunSwiftSrc(t *testing.T, swiftExe string, code []byte) ([]byte, error) {
	dir := t.TempDir()
	file := filepath.Join(dir, "main.swift")
	if err := os.WriteFile(file, code, 0644); err != nil {
		return nil, err
	}
	exe := filepath.Join(dir, "main")
	if out, err := exec.Command(swiftExe, file, "-o", exe).CombinedOutput(); err != nil {
		return out, err
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		return out, err
	}
	return out, nil
}

func TestSwiftTranspiler_PrintHello(t *testing.T) {
	swiftExe := ensureSwift(t)
	root := testutil.FindRepoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "swift")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "print_hello.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	ast, err := swifttrans.Transpile(env, prog)
	if err != nil {
		t.Fatalf("transpile error: %v", err)
	}
	code := ast.Emit()
	codePath := filepath.Join(outDir, "print_hello.swift")
	if err := os.WriteFile(codePath, code, 0644); err != nil {
		t.Fatal(err)
	}
	out, err := compileAndRunSwiftSrc(t, swiftExe, code)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "print_hello.error"), out, 0644)
		t.Fatalf("swift run error: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "print_hello.error"))
	got := bytes.TrimSpace(out)
	want, _ := os.ReadFile(filepath.Join(outDir, "print_hello.out"))
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch: got %s want %s", got, want)
	}
}
