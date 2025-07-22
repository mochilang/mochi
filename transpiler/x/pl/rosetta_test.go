//go:build slow

package pl_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	"mochi/parser"
	pl "mochi/transpiler/x/pl"
	"mochi/types"
)

func repoRootRosetta(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal("cannot determine working directory")
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

func shouldUpdateRosetta() bool {
	if v, ok := os.LookupEnv("UPDATE"); ok && (v == "1" || v == "true") {
		return true
	}
	return false
}

func runRosettaTask(t *testing.T, name string) {
	root := repoRootRosetta(t)
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Prolog")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "rosetta", "x", "Mochi", name+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := pl.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := pl.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	plFile := filepath.Join(outDir, name+".pl")
	if err := os.WriteFile(plFile, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("swipl", "-q", "-f", plFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil || bytes.Contains(out, []byte("ERROR:")) {
		_ = os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, name+".error"))
	outWant := filepath.Join(outDir, name+".out")
	if shouldUpdateRosetta() {
		_ = os.WriteFile(outWant, append(got, '\n'), 0o644)
	} else if want, err := os.ReadFile(outWant); err == nil {
		if !bytes.Equal(got, bytes.TrimSpace(want)) {
			t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, bytes.TrimSpace(want))
		}
	}
}

func TestPrologTranspiler_Rosetta(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRootRosetta(t)
	files, err := filepath.Glob(filepath.Join(root, "tests", "rosetta", "x", "Mochi", "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	sort.Strings(files)
	max := 20
	if len(files) < max {
		max = len(files)
	}
	for _, f := range files[:max] {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		t.Run(name, func(t *testing.T) { runRosettaTask(t, name) })
	}
	pl.UpdateRosettaReadme()
}
