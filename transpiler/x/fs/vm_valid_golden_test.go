//go:build slow

package fstrans_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"mochi/golden"
	"mochi/parser"
	fstrans "mochi/transpiler/x/fs"
	"mochi/types"
)

func TestFSTranspiler_VMValid_Golden(t *testing.T) {
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "fs")
	os.MkdirAll(outDir, 0o755)

	if _, err := exec.LookPath("fsharpc"); err != nil {
		t.Skip("fsharpc not installed")
	}
	if _, err := exec.LookPath("mono"); err != nil {
		t.Skip("mono not installed")
	}

	golden.RunWithSummary(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, base+".fs")
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
		ast, err := fstrans.Transpile(prog, env)
		if err != nil {
			_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
			return nil, err
		}
		code := fstrans.Emit(ast)
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return nil, err
		}
		exe := filepath.Join(outDir, base+".exe")
		cmd := exec.Command("fsharpc", "--target:exe", "--out:"+exe, codePath)
		if out, err := cmd.CombinedOutput(); err != nil {
			_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
			return nil, err
		}
		run := exec.Command("mono", exe)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			run.Stdin = bytes.NewReader(data)
		}
		out, err := run.CombinedOutput()
		if err != nil {
			_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
			return nil, err
		}
		outBytes := bytes.TrimSpace(out)
		_ = os.WriteFile(outPath, outBytes, 0o644)
		_ = os.Remove(errPath)
		return outBytes, nil
	})
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	updateTasks()
	os.Exit(code)
}

func updateReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "fs")
	readmePath := filepath.Join(root, "transpiler", "x", "fs", "README.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s.mochi", mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# Mochi F# Transpiler\n\n")
	buf.WriteString("This folder contains an experimental transpiler that converts Mochi source code into F#.\n\n")
	fmt.Fprintf(&buf, "## Golden Test Checklist (%d/%d)\n\n", compiled, total)
	buf.WriteString("The list below tracks Mochi programs under `tests/vm/valid` that should successfully transpile. Checked items indicate tests known to work.\n\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func updateTasks() {
	root := repoRoot(&testing.T{})
	taskFile := filepath.Join(root, "transpiler", "x", "fs", "TASKS.md")
	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := ""
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			ts = t.Format("2006-01-02 15:04 MST")
		}
	}
	var buf bytes.Buffer
	buf.WriteString(fmt.Sprintf("## Progress (%s)\n", ts))
	buf.WriteString("- VM valid golden test results updated\n\n")
	if data, err := os.ReadFile(taskFile); err == nil {
		buf.Write(data)
	}
	_ = os.WriteFile(taskFile, buf.Bytes(), 0o644)
}
