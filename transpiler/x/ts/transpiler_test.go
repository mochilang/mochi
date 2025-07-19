package tstranspiler_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/parser"
	tstranspiler "mochi/transpiler/x/ts"
	"mochi/types"
)

func repoRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	t.Fatal("go.mod not found")
	return ""
}

// TestTranspilePrintHello compiles the simple print_hello.mochi program to
// TypeScript, runs it through tsc+node and compares the output with the
// existing golden file.
func TestTranspilePrintHello(t *testing.T) {
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "vm", "valid", "print_hello.mochi")

	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}

	tsProg, err := tstranspiler.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile error: %v", err)
	}

	code := tstranspiler.Emit(tsProg)

	tmpDir := t.TempDir()
	tsFile := filepath.Join(tmpDir, "out.ts")
	if err := os.WriteFile(tsFile, code, 0o644); err != nil {
		t.Fatal(err)
	}
	jsDir := filepath.Join(tmpDir, "js")
	cmd := exec.Command("tsc", tsFile, "--outDir", jsDir)
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("tsc: %v\n%s", err, out)
	}
	jsFile := filepath.Join(jsDir, "out.js")
	out, err := exec.Command("node", jsFile).CombinedOutput()
	if err != nil {
		t.Fatalf("node: %v\n%s", err, out)
	}
	got := strings.TrimSpace(string(out))
	wantBytes, err := os.ReadFile(filepath.Join(root, "tests", "vm", "valid", "print_hello.out"))
	if err != nil {
		t.Fatal(err)
	}
	want := strings.TrimSpace(string(wantBytes))
	if got != want {
		t.Fatalf("unexpected output: got %q want %q", got, want)
	}

	// For reference ensure emitted code matches expectation using golden file
	goldenPath := filepath.Join(root, "tests", "transpiler", "x", "ts", "print_hello.ts")
	if *updateGolden {
		os.WriteFile(goldenPath, bytes.TrimSpace(code), 0o644)
	} else if data, err := os.ReadFile(goldenPath); err == nil {
		if !bytes.Equal(bytes.TrimSpace(code), bytes.TrimSpace(data)) {
			t.Errorf("generated TypeScript mismatch\n--- got ---\n%s\n--- want ---\n%s", code, data)
		}
	}
}

var updateGolden = func() *bool {
	b := false
	if v := os.Getenv("UPDATE_GOLDEN"); v == "1" {
		b = true
	}
	return &b
}()
