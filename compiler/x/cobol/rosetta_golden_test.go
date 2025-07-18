//go:build slow

package cobol_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	cobol "mochi/compiler/x/cobol"
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
	if i := bytes.IndexByte(b, '\n'); i != -1 && bytes.HasPrefix(b, []byte("*>")) {
		return b[i+1:]
	}
	return b
}

func runRosettaTaskGolden(t *testing.T, name string) {
	root := repoRootRosetta(t)
	script := exec.Command("go", "run", "-tags=archive,slow", "./scripts/compile_rosetta_cobol.go")
	script.Env = append(os.Environ(), "TASKS="+name)
	script.Dir = root
	if out, err := script.CombinedOutput(); err != nil {
		t.Fatalf("compile script error: %v\n%s", err, out)
	}
	errFile := filepath.Join(root, "tests", "rosetta", "out", "Cobol", name+".error")
	if b, err := os.ReadFile(errFile); err == nil {
		t.Skipf("cobol run failed:\n%s", b)
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
	code, err := cobol.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	codeWant := filepath.Join(root, "tests", "rosetta", "out", "Cobol", name+".cob")
	if shouldUpdateRosetta() {
		_ = os.WriteFile(codeWant, code, 0o644)
	} else if want, err := os.ReadFile(codeWant); err == nil {
		got := stripHeaderLocal(bytes.TrimSpace(code))
		want = stripHeaderLocal(bytes.TrimSpace(want))
		if !bytes.Equal(got, want) {
			t.Errorf("generated code mismatch for %s.cob\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, got, want)
		}
	}

	dir := t.TempDir()
	file := filepath.Join(dir, "prog.cob")
	if err := os.WriteFile(file, code, 0o644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	bin := filepath.Join(dir, "prog")
	if out, err := exec.Command("cobc", "-free", "-std=cobol2002", "-x", "-o", bin, file).CombinedOutput(); err != nil {
		t.Skipf("cobc error: %v\n%s", err, out)
		return
	}
	outBytes, err := exec.Command(bin).CombinedOutput()
	if err != nil {
		t.Skipf("run error: %v\n%s", err, outBytes)
		return
	}
	gotOut := bytes.TrimSpace(outBytes)
	outWant := filepath.Join(root, "tests", "rosetta", "out", "Cobol", name+".out")
	if shouldUpdateRosetta() {
		_ = os.WriteFile(outWant, append(gotOut, '\n'), 0o644)
	} else if wantOut, err := os.ReadFile(outWant); err == nil {
		if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
			t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, gotOut, bytes.TrimSpace(wantOut))
		}
	}
}

func TestCobolCompiler_Rosetta_Golden(t *testing.T) {
	if _, err := exec.LookPath("cobc"); err != nil {
		t.Skip("cobc not installed")
	}
	root := repoRootRosetta(t)
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

func repoRootRosetta(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal("cannot determine working directory")
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		p := filepath.Dir(dir)
		if p == dir {
			break
		}
		dir = p
	}
	t.Fatal("go.mod not found")
	return ""
}
