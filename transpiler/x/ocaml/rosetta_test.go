//go:build rosetta

package ocaml_test

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
	ocaml "mochi/transpiler/x/ocaml"
	"mochi/types"
)

func repoRootDir(t *testing.T) string {
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

func runCase(src, outDir string) ([]byte, error) {
	base := strings.TrimSuffix(filepath.Base(src), ".mochi")
	codePath := filepath.Join(outDir, base+".ml")
	outPath := filepath.Join(outDir, base+".out")
	errPath := filepath.Join(outDir, base+".error")

	prog, err := parser.Parse(src)
	if err != nil {
		_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
		return nil, err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		_ = os.WriteFile(errPath, []byte(errs[0].Error()), 0o644)
		return nil, errs[0]
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
		return nil, err
	}
	code := ast.Emit()
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		return nil, err
	}
	exe := filepath.Join(outDir, base)
	if out, err := exec.Command("ocamlc", codePath, "-o", exe).CombinedOutput(); err != nil {
		_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
		return nil, err
	}
	cmd := exec.Command(exe)
	cmd.Env = append(os.Environ(), "MOCHI_NOW_SEED=1")
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
		return nil, err
	}
	outBytes := bytes.TrimSpace(out)
	_ = os.WriteFile(outPath, outBytes, 0o644)
	_ = os.Remove(errPath)
	return outBytes, nil
}

func TestOCamlTranspiler_Rosetta_Golden(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRootDir(t)
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "OCaml")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(root, "tests", "rosetta", "x", "Mochi", "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	sort.Strings(files)
	if v := os.Getenv("ROSETTA_INDEX"); v != "" {
		if idx, err := strconv.Atoi(v); err == nil && idx >= 1 && idx <= len(files) {
			files = files[idx-1 : idx]
		} else {
			t.Fatalf("invalid ROSETTA_INDEX %s", v)
		}
	}
	var passed int
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		if ok := t.Run(name, func(t *testing.T) {
			if _, err := runCase(src, outDir); err != nil {
				t.Fatalf("%v", err)
			}
		}); !ok {
			t.Fatalf("first failing program: %s", name)
		}
		passed++
	}
	t.Logf("Summary: %d passed, %d failed", passed, len(files)-passed)
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateRosettaReadme()
	os.Exit(code)
}

func updateRosettaReadme() {
	root := repoRootDir(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "OCaml")
	readmePath := filepath.Join(root, "transpiler", "x", "ocaml", "ROSETTA.md")

	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
	total := len(files)
	completed := 0
	var lines []string
	for i, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			completed++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("%d. %s %s", i+1, mark, name))
	}

	var buf bytes.Buffer
	buf.WriteString("# Rosetta OCaml Transpiler\n\n")
	buf.WriteString("This directory contains OCaml code generated from Rosetta Code programs in `tests/rosetta/x/Mochi`.\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n\n", completed, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	if out, err := exec.Command("git", "log", "-1", "--format=%cI").Output(); err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			buf.WriteString("Last updated " + t.Format("2006-01-02 15:04 MST") + "\n")
		}
	}
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}
