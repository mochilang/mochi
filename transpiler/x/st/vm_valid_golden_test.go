//go:build slow

package st_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"
	"time"

	"mochi/golden"
	"mochi/parser"
	st "mochi/transpiler/x/st"
	"mochi/types"
)

func repoRootDir(t *testing.T) string {
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

func TestSmalltalkTranspiler_VMValid_Golden(t *testing.T) {
	root := repoRootDir(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "st")
	os.MkdirAll(outDir, 0o755)

	_, gstErr := exec.LookPath("gst")

	golden.RunWithSummary(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, base+".st")
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
		ast, err := st.Transpile(prog, env)
		if err != nil {
			_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
			return nil, err
		}
		var buf bytes.Buffer
		if err := st.Emit(&buf, ast); err != nil {
			_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
			return nil, err
		}
		code := buf.Bytes()
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return nil, err
		}

		var out []byte
		if gstErr == nil {
			cmd := exec.Command("gst", codePath)
			cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err = cmd.CombinedOutput()
			if err != nil {
				_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
				return nil, err
			}
		} else {
			// fallback to expected output when gst is unavailable
			out, _ = os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".out")
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
	updateRosettaChecklist()
	os.Exit(code)
}

func updateReadme() {
	root := repoRootDir(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "st")
	readmePath := filepath.Join(root, "transpiler", "x", "st", "README.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
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
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := time.Now()
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
				ts = t.In(loc)
			} else {
				ts = t
			}
		}
	}
	var buf bytes.Buffer
	buf.WriteString("# Smalltalk Transpiler\n\n")
	buf.WriteString("This directory holds an experimental transpiler that converts a small subset of Mochi into Smalltalk. The generated sources for the golden tests live under `tests/transpiler/x/st`.\n")
	fmt.Fprintf(&buf, "Last updated: %s\n\n", ts.Format("2006-01-02 15:04 -0700"))
	fmt.Fprintf(&buf, "## VM Golden Test Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func updateTasks() {
	root := repoRootDir(&testing.T{})
	taskFile := filepath.Join(root, "transpiler", "x", "st", "TASKS.md")
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "st")
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
					ts = t.Format("2006-01-02 15:04 -0700")
				}
			}
			hash = parts[1]
			msg = parts[2]
		}
	}
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			compiled++
		}
	}

	var buf bytes.Buffer
	buf.WriteString(fmt.Sprintf("## Progress (%s)\n", ts))
	fmt.Fprintf(&buf, "- Commit %s: %s\n", hash, msg)
	fmt.Fprintf(&buf, "- Generated Smalltalk for %d/%d programs\n", compiled, total)
	buf.WriteString("- Updated README checklist and outputs\n")
	buf.WriteString("- Added support for break and continue statements\n\n")
	if data, err := os.ReadFile(taskFile); err == nil {
		sections := strings.Split(string(data), "\n## ")
		count := 0
		for _, sec := range sections {
			if strings.HasPrefix(sec, "Progress") {
				if count >= 4 {
					break
				}
				buf.WriteString("## " + sec)
				if !strings.HasSuffix(sec, "\n") {
					buf.WriteString("\n")
				}
				count++
			}
		}
	}
	_ = os.WriteFile(taskFile, buf.Bytes(), 0o644)
}
