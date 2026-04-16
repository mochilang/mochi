//go:build slow

package cobol_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	cobol "mochi/transpiler/x/cobol"
	"mochi/types"
)

func repoRootRosetta(t *testing.T) string {
	t.Helper()
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
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

func runRosettaCase(t *testing.T, name string) {
	root := repoRootRosetta(t)
	src := filepath.Join(root, "tests", "rosetta", "x", "Mochi", name+".mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "COBOL")
	os.MkdirAll(outDir, 0o755)

	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := cobol.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := cobol.Emit(ast)
	codePath := filepath.Join(outDir, name+".cob")
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	exe := filepath.Join(outDir, name)
	if out, err := exec.Command("cobc", "-free", codePath, "-x", "-o", exe).CombinedOutput(); err != nil {
		_ = os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
		t.Fatalf("compile: %v", err)
	}
	cmd := exec.Command(exe)
	if in, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(in)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	wantPath := filepath.Join(outDir, name+".out")
	want, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatalf("missing output: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, got, want)
	}
	_ = os.Remove(filepath.Join(outDir, name+".error"))
}

func TestCOBOLTranspiler_Rosetta(t *testing.T) {
	defer updateChecklist()
	if err := cobol.EnsureCOBOL(); err != nil {
		t.Skip("cobc not installed")
	}
	root := repoRootRosetta(t)
	files, err := filepath.Glob(filepath.Join(root, "tests", "rosetta", "x", "Mochi", "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	sort.Strings(files)
	max := 3
	if v := os.Getenv("ROSETTA_MAX"); v != "" {
		if n, err := strconv.Atoi(v); err == nil {
			max = n
		}
	}
	if len(files) < max {
		max = len(files)
	}
	for _, f := range files[:max] {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		t.Run(name, func(t *testing.T) { runRosettaCase(t, name) })
	}
}

func updateChecklist() {
	root := repoRootRosetta(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "COBOL")
	readme := filepath.Join(root, "transpiler", "x", "cobol", "ROSETTA.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			if _, err2 := os.Stat(filepath.Join(outDir, name+".error")); os.IsNotExist(err2) {
				compiled++
				mark = "[x]"
			}
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	ts := ""
	if out, err := exec.Command("git", "log", "-1", "--format=%cI").Output(); err == nil {
		if t, err := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); err == nil {
			if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
				ts = t.In(loc).Format("2006-01-02 15:04 -0700")
			} else {
				ts = t.Format("2006-01-02 15:04 -0700")
			}
		}
	}
	var buf bytes.Buffer
	buf.WriteString("# COBOL Rosetta Transpiler\n\n")
	buf.WriteString("Generated COBOL code for Rosetta tasks lives under `tests/rosetta/transpiler/COBOL`.\n\n")
	fmt.Fprintf(&buf, "## Rosetta Checklist (%d/%d) - updated %s\n", compiled, total, ts)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readme, buf.Bytes(), 0o644)
}
