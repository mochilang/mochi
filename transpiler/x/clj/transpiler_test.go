package cljt_test

import (
	"bytes"
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"mochi/parser"
	cljt "mochi/transpiler/x/clj"
	"mochi/types"
)

var update = flag.Bool("update", false, "update golden files")

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func findRepoRoot(t *testing.T) string {
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

// EnsureClojure checks that the clojure CLI tool is installed.
func EnsureClojure() error {
	if _, err := exec.LookPath("clojure"); err == nil {
		return nil
	}
	if _, err := exec.LookPath("clj"); err == nil {
		return nil
	}
	return exec.ErrNotFound
}

func TestTranspile_PrintHello(t *testing.T) {
	if err := EnsureClojure(); err != nil {
		t.Skip("clojure not installed")
	}
	root := findRepoRoot(t)
	src := filepath.Join(root, "tests", "vm", "valid", "print_hello.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatal(err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := cljt.Transpile(prog, env)
	if err != nil {
		t.Fatal(err)
	}
	code := cljt.EmitString(ast)

	outDir := filepath.Join(root, "tests", "transpiler", "x", "clj")
	os.MkdirAll(outDir, 0o755)
	cljFile := filepath.Join(outDir, "print_hello.clj")
	if err := os.WriteFile(cljFile, code, 0o644); err != nil {
		t.Fatal(err)
	}

	cmd := exec.Command("clojure", cljFile)
	var out bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &out
	if err := cmd.Run(); err != nil {
		t.Fatalf("run error: %v: %s", err, out.String())
	}
	got := bytes.TrimSpace(out.Bytes())
	want := []byte("hello")
	if !bytes.Equal(got, want) {
		t.Fatalf("unexpected output: %s", got)
	}
}

func TestTranspile_Golden(t *testing.T) {
	root := findRepoRoot(t)
	src := filepath.Join(root, "tests", "vm", "valid", "print_hello.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatal(err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := cljt.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	got := cljt.EmitString(ast)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "clj")
	os.MkdirAll(outDir, 0o755)
	path := filepath.Join(outDir, "print_hello.clj")
	if shouldUpdate() {
		if err := os.WriteFile(path, got, 0o644); err != nil {
			t.Fatalf("write: %v", err)
		}
		return
	}
	want, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if !bytes.Equal(bytes.TrimSpace(got), bytes.TrimSpace(want)) {
		t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", got, want)
	}
}
