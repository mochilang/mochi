//go:build rosetta

package erl_test

import (
	"bytes"
	"flag"
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
	erl "mochi/transpiler/x/erl"
	"mochi/types"
)

var update = flag.Bool("update", false, "update golden files")

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

func updateEnabled() bool { return *update }

func TestRosettaTranspile(t *testing.T) {
	if _, err := exec.LookPath("escript"); err != nil {
		t.Skip("escript not installed")
	}
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "erl")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("list sources: %v", err)
	}
	sort.Strings(files)
	if len(files) == 0 {
		t.Fatal("no mochi files found")
	}

	if idxStr := os.Getenv("MOCHI_ROSETTA_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 || idx > len(files) {
			t.Fatalf("invalid MOCHI_ROSETTA_INDEX: %s", idxStr)
		}
		files = []string{files[idx-1]}
	} else {
		max := len(files)
		if v := os.Getenv("ROSETTA_MAX"); v != "" {
			if n, err := strconv.Atoi(v); err == nil && n < max {
				max = n
			}
		}
		files = files[:max]
	}

	var firstFail string
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		ok := t.Run(name, func(t *testing.T) { runRosetta(t, src, name, outDir) })
		if !ok && firstFail == "" {
			firstFail = name
			break
		}
	}
	if firstFail != "" {
		t.Fatalf("first failing program: %s", firstFail)
	}
}

func runRosetta(t *testing.T, srcPath, name, outDir string) {
	erlFile := filepath.Join(outDir, name+".erl")
	outFile := filepath.Join(outDir, name+".out")
	errFile := filepath.Join(outDir, name+".error")

	prog, err := parser.Parse(srcPath)
	if err != nil {
		_ = os.WriteFile(errFile, []byte(fmt.Sprintf("parse error: %v", err)), 0o644)
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		_ = os.WriteFile(errFile, []byte(fmt.Sprintf("type error: %v", errs[0])), 0o644)
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := erl.Transpile(prog, env)
	if err != nil {
		_ = os.WriteFile(errFile, []byte(fmt.Sprintf("transpile error: %v", err)), 0o644)
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	if updateEnabled() {
		if err := os.WriteFile(erlFile, code, 0o644); err != nil {
			t.Fatalf("write erl: %v", err)
		}
	} else {
		_ = os.WriteFile(erlFile, code, 0o644)
	}

	cmd := exec.Command("escript", erlFile)
	cmd.Env = append(os.Environ(), "MOCHI_NOW_SEED=1")
	if data, err := os.ReadFile(strings.TrimSuffix(srcPath, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	var buf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = &buf
	if err := cmd.Run(); err != nil {
		if updateEnabled() {
			_ = os.WriteFile(errFile, append([]byte(err.Error()+"\n"), buf.Bytes()...), 0o644)
		}
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(buf.Bytes())
	if updateEnabled() {
		_ = os.WriteFile(outFile, got, 0o644)
		_ = os.Remove(errFile)
		return
	}
	want, err := os.ReadFile(outFile)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
	_ = os.Remove(errFile)
}

func countRosetta() (int, int) {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "erl")
	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		return 0, 0
	}
	total := len(files)
	compiled := 0
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			compiled++
		}
	}
	return compiled, total
}

func updateRosettaReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "erl")
	readmePath := filepath.Join(root, "transpiler", "x", "erl", "ROSETTA.md")
	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		return
	}
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	for i, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %d. %s", mark, i+1, name))
	}
	tsRaw, _ := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := strings.TrimSpace(string(tsRaw))
	if t, err := time.Parse(time.RFC3339, ts); err == nil {
		if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
			ts = t.In(loc).Format("2006-01-02 15:04 -0700")
		} else {
			ts = t.Format("2006-01-02 15:04 MST")
		}
	}
	var buf bytes.Buffer
	fmt.Fprintf(&buf, "# Erlang Rosetta Output (%d/%d compiled and run)\n", compiled, total)
	fmt.Fprintf(&buf, "Last updated: %s\n\n", ts)
	buf.WriteString("This directory contains Erlang code generated by the Mochi transpiler for Rosetta Code tasks. Each program in `tests/rosetta/x/Mochi` is transpiled and executed with `escript`.\n\n")
	buf.WriteString("## Program checklist\n\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateRosettaReadme()
	os.Exit(code)
}
