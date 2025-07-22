//go:build slow

package pas_test

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
	pas "mochi/transpiler/x/pas"
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

func ensureFPCRosetta(t *testing.T) string {
	t.Helper()
	path, err := exec.LookPath("fpc")
	if err != nil {
		t.Skip("fpc not installed")
	}
	return path
}

func runRosettaCase(t *testing.T, name string) {
	t.Helper()
	fpc := ensureFPCRosetta(t)
	root := repoRootRosetta(t)
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Pascal")
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
	ast, err := pas.Transpile(env, prog)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	codePath := filepath.Join(outDir, name+".pas")
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		t.Fatal(err)
	}
	exe := filepath.Join(outDir, name)
	if out, err := exec.Command(fpc, codePath, "-o"+exe).CombinedOutput(); err != nil {
		_ = os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
		t.Fatalf("compile: %v", err)
	}
	cmd := exec.Command(exe)
	if in, err := os.ReadFile(filepath.Join(root, "tests", "rosetta", "x", "Mochi", name+".in")); err == nil {
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
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("%s output mismatch: got %s want %s", name, got, want)
	}
	_ = os.Remove(filepath.Join(outDir, name+".error"))
}

func listRosettaPrograms(t *testing.T) []string {
        t.Helper()
        root := repoRootRosetta(t)
        pattern := filepath.Join(root, "tests", "rosetta", "x", "Mochi", "*.mochi")
        files, err := filepath.Glob(pattern)
        if err != nil {
                t.Fatalf("glob: %v", err)
        }
        sort.Strings(files)
        programs := make([]string, len(files))
        for i, f := range files {
                programs[i] = strings.TrimSuffix(filepath.Base(f), ".mochi")
        }
        return programs
}

func TestPascalTranspiler_Rosetta(t *testing.T) {
        if _, err := exec.LookPath("fpc"); err != nil {
                t.Skip("fpc not installed")
        }
        programs := listRosettaPrograms(t)
        if len(programs) == 0 {
                t.Fatalf("no rosetta programs found")
        }

        idx := 1
        if v := os.Getenv("ROSETTA_INDEX"); v != "" {
                if n, err := strconv.Atoi(v); err == nil {
                        idx = n
                }
        }
        if idx < 1 || idx > len(programs) {
                t.Fatalf("index %d out of range (1-%d)", idx, len(programs))
        }
        name := programs[idx-1]
        t.Logf("running %s (index %d)", name, idx)
        runRosettaCase(t, name)
}

func updateRosettaChecklist() {
	root := repoRootRosetta(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Pascal")
	readmePath := filepath.Join(root, "transpiler", "x", "pas", "ROSETTA.md")

	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
	total := len(files)
	completed := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			if _, err2 := os.Stat(filepath.Join(outDir, name+".error")); os.IsNotExist(err2) {
				completed++
				mark = "[x]"
			}
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# Pascal Rosetta Transpiler\n\n")
	buf.WriteString("Generated Pascal code for Rosetta tasks lives under `tests/rosetta/transpiler/Pascal`.\n\n")
	ts := time.Now().Format("2006-01-02 15:04 MST")
	if out, err := exec.Command("git", "log", "-1", "--format=%cI").Output(); err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			ts = t.Format("2006-01-02 15:04 MST")
		}
	}
	fmt.Fprintf(&buf, "## Rosetta Checklist (%d/%d) - updated %s\n", completed, total, ts)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}
