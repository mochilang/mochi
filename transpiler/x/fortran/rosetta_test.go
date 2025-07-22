//go:build slow

package fortran_test

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
	ftn "mochi/transpiler/x/fortran"
	"mochi/types"
)

func runTask(name string) ([]byte, error) {
	root := repoRoot(&testing.T{})
	src := filepath.Join(root, "tests", "rosetta", "x", "Mochi", name+".mochi")

	prog, err := parser.Parse(src)
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type: %v", errs[0])
	}
	ast, err := ftn.Transpile(prog, env)
	if err != nil {
		return nil, fmt.Errorf("transpile: %w", err)
	}

	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Fortran")
	os.MkdirAll(outDir, 0o755)

	f90Path := filepath.Join(outDir, name+".f90")
	if err := os.WriteFile(f90Path, ast.Emit(), 0o644); err != nil {
		return nil, err
	}
	exe := filepath.Join(outDir, name)
	if out, err := exec.Command("gfortran", f90Path, "-o", exe).CombinedOutput(); err != nil {
		_ = os.WriteFile(filepath.Join(outDir, name+".error"), append([]byte(err.Error()+"\n"), out...), 0o644)
		return nil, fmt.Errorf("compile: %v", err)
	}
	cmd := exec.Command(exe)
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, name+".error"), append([]byte(err.Error()+"\n"), out...), 0o644)
		return nil, fmt.Errorf("run: %v", err)
	}
	outBytes := bytes.TrimSpace(out)
	_ = os.WriteFile(filepath.Join(outDir, name+".out"), outBytes, 0o644)
	_ = os.Remove(filepath.Join(outDir, name+".error"))
	return outBytes, nil
}

func TestFortranTranspiler_Rosetta(t *testing.T) {
	if _, err := exec.LookPath("gfortran"); err != nil {
		t.Skip("gfortran not installed")
	}
	root := repoRoot(t)

	names, err := listPrograms(root)
	if err != nil {
		t.Fatalf("list programs: %v", err)
	}

	idx := 1
	if v := os.Getenv("ROSETTA_INDEX"); v != "" {
		if n, err := strconv.Atoi(v); err == nil {
			idx = n
		}
	}
	if idx < 1 || idx > len(names) {
		t.Fatalf("index %d out of range (1-%d)", idx, len(names))
	}
	name := names[idx-1]
	t.Logf("running #%d: %s", idx, name)

	got, err := runTask(name)
	if err != nil {
		t.Fatalf("%v", err)
	}
	wantPath := filepath.Join(root, "tests", "rosetta", "x", "Mochi", name+".out")
	if want, err := os.ReadFile(wantPath); err == nil {
		if !bytes.Equal(got, bytes.TrimSpace(want)) {
			t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, got, bytes.TrimSpace(want))
		}
	}
}

func listPrograms(root string) ([]string, error) {
	files, err := filepath.Glob(filepath.Join(root, "tests", "rosetta", "x", "Mochi", "*.mochi"))
	if err != nil {
		return nil, err
	}
	sort.Strings(files)
	names := make([]string, len(files))
	for i, f := range files {
		names[i] = strings.TrimSuffix(filepath.Base(f), ".mochi")
	}
	return names, nil
}

func updateChecklist() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Fortran")
	readme := filepath.Join(root, "transpiler", "x", "fortran", "ROSETTA.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	for i, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("%d. %s %s", i+1, mark, name))
	}
	ts := ""
	if out, err := exec.Command("git", "log", "-1", "--format=%cI").Output(); err == nil {
		if t, err := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); err == nil {
			if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
				ts = t.In(loc).Format("2006-01-02 15:04:05 -0700")
			} else {
				ts = t.Format("2006-01-02 15:04:05 -0700")
			}
		}
	}
	var buf bytes.Buffer
	buf.WriteString("# Rosetta Fortran Transpiler Checklist\n\n")
	buf.WriteString("This checklist tracks Mochi programs from `tests/rosetta/x/Mochi` that successfully transpile using the experimental Fortran backend.\n\n")
	fmt.Fprintf(&buf, "Checklist of programs that currently transpile and run (%d/%d):\n\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	if ts != "" {
		fmt.Fprintf(&buf, "\n_Last updated: %s_\n", ts)
	}
	_ = os.WriteFile(readme, buf.Bytes(), 0o644)
}
