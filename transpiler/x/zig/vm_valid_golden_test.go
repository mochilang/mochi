//go:build slow

package zigt_test

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
	zigt "mochi/transpiler/x/zig"
	"mochi/types"
)

func TestZigTranspiler_VMValid_Golden(t *testing.T) {
	if _, err := exec.LookPath("zig"); err != nil {
		t.Skip("zig not installed")
	}
	root := vmRepoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "zig")
	os.MkdirAll(outDir, 0o755)

	golden.RunWithSummary(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, base+".zig")
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
		ast, err := zigt.Transpile(prog, env, false)
		if err != nil {
			_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
			return nil, err
		}
		code := ast.Emit()
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return nil, err
		}
		cmd := exec.Command("zig", "run", codePath)
		cmd.Env = append(os.Environ(), "MOCHI_NOW_SEED=1")
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

func vmRepoRoot(t *testing.T) string {
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
	root := vmRepoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "zig")
	readmePath := filepath.Join(root, "transpiler", "x", "zig", "README.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
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
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := filepath.Base(f)
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, strings.TrimSuffix(name, ".mochi")+".zig")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# Zig Transpiler\n\n")
	buf.WriteString("Generated Zig code for the Mochi VM valid tests lives under `tests/transpiler/x/zig`.\n\n")
	if ts != "" {
		fmt.Fprintf(&buf, "Last updated: %s\n\n", ts)
	}
	fmt.Fprintf(&buf, "## VM Golden Test Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func updateTasks() {
	root := vmRepoRoot(&testing.T{})
	taskFile := filepath.Join(root, "transpiler", "x", "zig", "TASKS.md")
	out, err := exec.Command("git", "log", "-1", "--format=%cI%n%h%n%s").Output()
	ts := ""
	hash := ""
	msg := ""
	if err == nil {
		parts := strings.SplitN(strings.TrimSpace(string(out)), "\n", 3)
		if len(parts) == 3 {
			if t, perr := time.Parse(time.RFC3339, parts[0]); perr == nil {
				if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
					ts = t.In(loc).Format("2006-01-02 15:04 -0700")
				} else {
					ts = t.Format("2006-01-02 15:04 MST")
				}
			}
			hash = parts[1]
			msg = parts[2]
		}
	}
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "zig")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	for _, f := range files {
		name := filepath.Base(f)
		if _, err := os.Stat(filepath.Join(outDir, strings.TrimSuffix(name, ".mochi")+".zig")); err == nil {
			compiled++
		}
	}
	var buf bytes.Buffer
	buf.WriteString(fmt.Sprintf("## Progress (%s)\n", ts))
	fmt.Fprintf(&buf, "- Commit %s: %s\n", hash, msg)
	fmt.Fprintf(&buf, "- Generated Zig for %d/%d programs\n", compiled, total)
	buf.WriteString("- Updated README checklist and outputs\n\n")
	if data, err := os.ReadFile(taskFile); err == nil {
		buf.Write(data)
	}
	_ = os.WriteFile(taskFile, buf.Bytes(), 0o644)
}
