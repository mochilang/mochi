//go:build slow

package rs_test

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
	rs "mochi/transpiler/x/rs"
	"mochi/types"
)

func TestRustTranspiler_VMValid_Golden(t *testing.T) {
	if _, err := exec.LookPath("rustc"); err != nil {
		t.Skip("rustc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "rs")
	os.MkdirAll(outDir, 0o755)

	golden.RunWithSummary(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, base+".rs")
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
		ast, err := rs.Transpile(prog, env)
		if err != nil {
			_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
			return nil, err
		}
		code := rs.Emit(ast)
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return nil, err
		}
		tmp := t.TempDir()
		bin := filepath.Join(tmp, base)
		if out, err := exec.Command("rustc", codePath, "-O", "-o", bin).CombinedOutput(); err != nil {
			_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
			return nil, err
		}
		cmd := exec.Command(bin)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		got := bytes.TrimSpace(out)
		if err != nil {
			_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
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

func updateReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "rs")
	readmePath := filepath.Join(root, "transpiler", "x", "rs", "README.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".rs")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# Mochi to Rust transpiler\n\n")
	buf.WriteString("This experimental transpiler converts a tiny subset of Mochi into Rust code.\n\n")
	fmt.Fprintf(&buf, "## Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func updateTasks() {
	root := repoRoot(&testing.T{})
	taskFile := filepath.Join(root, "transpiler", "x", "rs", "TASKS.md")
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
	buf.WriteString("- Added support for `append`, `avg` and `count` builtins.\n")
	buf.WriteString("- Regenerated VM golden outputs and updated README.\n\n")
	if data, err := os.ReadFile(taskFile); err == nil {
		buf.Write(data)
	}
	_ = os.WriteFile(taskFile, buf.Bytes(), 0o644)
}
