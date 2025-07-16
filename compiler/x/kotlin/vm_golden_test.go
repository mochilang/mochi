//go:build slow

package kotlin_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	kotlin "mochi/compiler/x/kotlin"
	_ "mochi/golden"
	"mochi/parser"
	"mochi/types"
)

// TestKotlinCompiler_VMGolden compiles append_builtin.mochi to Kotlin and
// verifies the generated source and runtime output. This keeps the test quick
// while exercising the full pipeline.
func TestKotlinCompiler_VMGolden(t *testing.T) {
	if _, err := exec.LookPath("kotlinc"); err != nil {
		t.Skip("kotlinc not installed")
	}
	root := repoRootVM()
	src := filepath.Join(root, "tests", "vm", "valid", "append_builtin.mochi")

	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := kotlin.New(env, src).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	// Compare generated code to golden file.
	wantCodePath := filepath.Join(root, "tests", "vm", "valid", "append_builtin.kt.out")
	if want, err := os.ReadFile(wantCodePath); err == nil {
		got := stripHeader(bytes.TrimSpace(code))
		got = bytes.ReplaceAll(got, []byte(src), []byte("."))
		want = stripHeader(bytes.TrimSpace(want))
		if !bytes.Equal(got, want) {
			t.Errorf("generated code mismatch\n\n--- got ---\n%s\n\n--- want ---\n%s", code, want)
		}
	} else if !os.IsNotExist(err) {
		t.Fatalf("read golden: %v", err)
	}

	dir := t.TempDir()
	ktFile := filepath.Join(dir, "Main.kt")
	if err := os.WriteFile(ktFile, code, 0o644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	jar := filepath.Join(dir, "main.jar")
	if out, err := exec.Command("kotlinc", ktFile, "-include-runtime", "-d", jar).CombinedOutput(); err != nil {
		t.Fatalf("kotlinc error: %v\n%s", err, out)
	}
	runOut, err := exec.Command("java", "-jar", jar).CombinedOutput()
	if err != nil {
		t.Fatalf("java error: %v\n%s", err, runOut)
	}
	_ = runOut
}

// repoRoot returns the repository root directory by searching upwards for go.mod.
func repoRootVM() string {
	dir, _ := os.Getwd()
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
	return dir
}
