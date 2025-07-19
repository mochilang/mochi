package tstranspiler_test

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
	meta "mochi/transpiler/meta"
	tstrans "mochi/transpiler/x/ts"
	"mochi/types"
)

func TestTSTranspiler_VMValid_Golden(t *testing.T) {
	if err := meta.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ts")
	os.MkdirAll(outDir, 0o755)

	golden.RunWithSummary(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, base+".ts")
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

		tsProg, err := tstrans.Transpile(prog, env)
		if err != nil {
			_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
			return nil, err
		}
		code := tstrans.Emit(tsProg)
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return nil, err
		}
		cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", codePath)
		cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		got := bytes.TrimSpace(out)
		if err != nil {
			_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
			return nil, err
		}
		_ = os.Remove(errPath)
		_ = os.WriteFile(outPath, got, 0o644)
		return got, nil
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
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ts")
	readmePath := filepath.Join(root, "transpiler", "x", "ts", "README.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := filepath.Base(f)
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, strings.TrimSuffix(name, ".mochi")+".ts")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# Mochi \u2192 TypeScript Transpiler\n\n")
	buf.WriteString("This directory contains the experimental TypeScript transpiler.\n")
	buf.WriteString("Generated sources for the golden tests live under `tests/transpiler/x/ts`.\n\n")
	fmt.Fprintf(&buf, "## VM Golden Test Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func updateTasks() {
	root := repoRoot(&testing.T{})
	taskFile := filepath.Join(root, "transpiler", "x", "ts", "TASKS.md")
	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := ""
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
				ts = t.In(loc).Format("2006-01-02 15:04 -0700")
			} else {
				ts = t.Format("2006-01-02 15:04 MST")
			}
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
