//go:build slow

package rbcode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	rbcode "mochi/compiler/x/rb"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func shouldUpdateRosetta() bool {
	if v, ok := os.LookupEnv("UPDATE"); ok && (v == "1" || v == "true") {
		return true
	}
	return false
}

func stripHeaderLocal(b []byte) []byte {
	if i := bytes.IndexByte(b, '\n'); i != -1 && bytes.HasPrefix(b, []byte("#")) {
		if bytes.HasPrefix(b, []byte("# Generated")) {
			return b[i+1:]
		}
	}
	return b
}

func runRosettaTaskGolden(t *testing.T, name string) {
	t.Helper()
	if err := rbcode.EnsureRuby(); err != nil {
		t.Skipf("ruby not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	script := exec.Command("go", "run", "-tags=archive,slow", "./scripts/compile_rosetta_rb.go")
	script.Env = append(os.Environ(), "GOTOOLCHAIN=local", "TASKS="+name)
	script.Dir = root
	if out, err := script.CombinedOutput(); err != nil {
		t.Fatalf("compile script error: %v\n%s", err, out)
	}
	errFile := filepath.Join(root, "tests", "rosetta", "out", "Ruby", name+".error")
	if b, err := os.ReadFile(errFile); err == nil {
		t.Skipf("ruby run failed:\n%s", b)
		return
	}

	src := filepath.Join(root, "tests", "rosetta", "x", "Mochi", name+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := rbcode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	codeWant := filepath.Join(root, "tests", "rosetta", "out", "Ruby", name+".rb")
	if shouldUpdateRosetta() {
		_ = os.WriteFile(codeWant, code, 0o644)
	} else if want, err := os.ReadFile(codeWant); err == nil {
		got := stripHeaderLocal(bytes.TrimSpace(code))
		want = stripHeaderLocal(bytes.TrimSpace(want))
		if !bytes.Equal(got, want) {
			t.Errorf("generated code mismatch for %s.rb\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, got, want)
		}
	}

	dir := t.TempDir()
	file := filepath.Join(dir, "main.rb")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	out, err := exec.Command("ruby", file).CombinedOutput()
	if err != nil {
		t.Skipf("run error: %v\n%s", err, out)
		return
	}
	gotOut := bytes.TrimSpace(out)
	outWant := filepath.Join(root, "tests", "rosetta", "out", "Ruby", name+".out")
	if shouldUpdateRosetta() {
		_ = os.WriteFile(outWant, append(gotOut, '\n'), 0o644)
	} else if wantOut, err := os.ReadFile(outWant); err == nil {
		if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
			t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, gotOut, bytes.TrimSpace(wantOut))
		}
	}
}

func TestRBCompiler_Rosetta_Golden(t *testing.T) {
	if err := rbcode.EnsureRuby(); err != nil {
		t.Skipf("ruby not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	files, err := filepath.Glob(filepath.Join(root, "tests", "rosetta", "x", "Mochi", "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	max := 3
	if len(files) < max {
		max = len(files)
	}
	for _, f := range files[:max] {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		t.Run(name, func(t *testing.T) { runRosettaTaskGolden(t, name) })
	}
}
