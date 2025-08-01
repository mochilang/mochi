//go:build slow

package cobol_test

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
	cobol "mochi/transpiler/x/cobol"
	"mochi/types"
)

func TestCOBOLTranspiler_VMValid_Golden(t *testing.T) {
	if err := cobol.EnsureCOBOL(); err != nil {
		t.Skip("cobc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "cobol")
	os.MkdirAll(outDir, 0o755)

	golden.RunWithSummary(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, base+".cob")
		outPath := filepath.Join(outDir, base+".out")
		errPath := filepath.Join(outDir, base+".error")

		prog, err := parser.Parse(src)
		if err != nil {
			_ = os.WriteFile(errPath, []byte("parse: "+err.Error()), 0o644)
			return nil, err
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			_ = os.WriteFile(errPath, []byte("type: "+errs[0].Error()), 0o644)
			return nil, errs[0]
		}
		ast, err := cobol.Transpile(prog, env)
		if err != nil {
			_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
			return nil, err
		}
		code := cobol.Emit(ast)
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return nil, err
		}
		exe := filepath.Join(outDir, base)
		if out, err := exec.Command("cobc", "-free", codePath, "-x", "-o", exe).CombinedOutput(); err != nil {
			_ = os.WriteFile(errPath, append([]byte("compile: "+err.Error()+"\n"), out...), 0o644)
			return nil, err
		}
		out, err := exec.Command(exe).CombinedOutput()
		_ = os.Remove(exe)
		if err != nil {
			_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
			return nil, err
		}
		_ = os.Remove(errPath)
		out = bytes.TrimSpace(out)
		_ = os.WriteFile(outPath, out, 0o644)
		return out, nil
	})
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	updateTasks()
	os.Exit(code)
}

func repoRoot(t *testing.T) string {
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

func updateReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "cobol")
	readmePath := filepath.Join(root, "transpiler", "x", "cobol", "README.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".cob")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
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
	buf.WriteString("# COBOL Transpiler\n\n")
	buf.WriteString("This directory stores COBOL code generated from Mochi programs in `tests/vm/valid`.\n")
	buf.WriteString("Each program is transpiled and the resulting `.cob` sources are compiled with `cobc` during testing.\n\n")
	fmt.Fprintf(&buf, "## VM Golden Test Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	buf.WriteString(fmt.Sprintf("Last updated: %s\n", ts))
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func updateTasks() {
	root := repoRoot(&testing.T{})
	taskFile := filepath.Join(root, "transpiler", "x", "cobol", "TASKS.md")
	tsRaw, _ := exec.Command("git", "log", "-1", "--format=%cI").Output()
	msgRaw, _ := exec.Command("git", "log", "-1", "--format=%s").Output()
	ts := strings.TrimSpace(string(tsRaw))
	if t, err := time.Parse(time.RFC3339, ts); err == nil {
		if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
			ts = t.In(loc).Format("2006-01-02 15:04 -0700")
		} else {
			ts = t.Format("2006-01-02 15:04 MST")
		}
	}
	msg := strings.TrimSpace(string(msgRaw))
	files, _ := filepath.Glob(filepath.Join(root, "tests", "transpiler", "x", "cobol", "*.cob"))
	compiled := len(files)
	srcFiles, _ := filepath.Glob(filepath.Join(root, "tests", "vm", "valid", "*.mochi"))
	total := len(srcFiles)

	var buf bytes.Buffer
	buf.WriteString(fmt.Sprintf("## Progress (%s)\n", ts))
	if msg != "" {
		buf.WriteString("- " + msg + "\n")
	}
	fmt.Fprintf(&buf, "- Generated COBOL for %d/%d programs\n", compiled, total)
	buf.WriteString("- Updated README checklist and outputs\n\n")
	if data, err := os.ReadFile(taskFile); err == nil {
		buf.Write(data)
	}
	_ = os.WriteFile(taskFile, buf.Bytes(), 0o644)
}
