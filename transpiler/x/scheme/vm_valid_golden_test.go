//go:build slow

package scheme_test

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
	scheme "mochi/transpiler/x/scheme"
	"mochi/types"
)

func TestSchemeTranspiler_VMValid_Golden(t *testing.T) {
	if _, err := exec.LookPath("chibi-scheme"); err != nil {
		t.Skip("scheme not installed")
	}
	root := findRepoRoot2(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "scheme")
	os.MkdirAll(outDir, 0o755)

	golden.RunWithSummary(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, base+".scm")
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
		bench := os.Getenv("MOCHI_BENCHMARK") != ""
		scheme.SetBenchMain(bench)
		lp, err := scheme.Transpile(prog, env)
		if err != nil {
			_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
			return nil, err
		}
		code := scheme.Format(scheme.EmitString(lp))
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return nil, err
		}
		cmd := exec.Command("chibi-scheme", "-q", "-m", "chibi", "-m", "srfi.1", "-m", "srfi.69", "-m", "scheme.sort", "-m", "chibi.string", codePath)
		runEnv := append(os.Environ())
		if bench {
			runEnv = append(runEnv, "MOCHI_BENCHMARK=1")
		} else {
			runEnv = append(runEnv, "MOCHI_NOW_SEED=1")
		}
		cmd.Env = runEnv
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
	})
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	updateTasks()
	updateRosettaChecklist()
	os.Exit(code)
}

func findRepoRoot2(t *testing.T) string {
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
	root := findRepoRoot2(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	binDir := filepath.Join(root, "tests", "transpiler", "x", "scheme")
	readmePath := filepath.Join(root, "transpiler", "x", "scheme", "README.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(binDir, name+".out")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	ts := ""
	if out, err := exec.Command("git", "log", "-1", "--format=%cI").Output(); err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			ts = t.UTC().Format("2006-01-02 15:04 UTC")
		}
	}

	var buf bytes.Buffer
	buf.WriteString("# Scheme Transpiler Output\n\n")
	buf.WriteString("Generated Scheme code for programs in `tests/vm/valid`. Each program has a `.scm` file produced by the transpiler and a `.out` file with its runtime output. Compilation or execution errors are captured in `.error` files.\n\n")
	fmt.Fprintf(&buf, "## VM Golden Test Checklist (%d/%d)\n", compiled, total)
	if ts != "" {
		fmt.Fprintf(&buf, "Last updated: %s\n\n", ts)
	} else {
		buf.WriteString("\n")
	}
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func updateTasks() {
	root := findRepoRoot2(&testing.T{})
	taskFile := filepath.Join(root, "transpiler", "x", "scheme", "TASKS.md")

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

	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "scheme")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
	if len(files) > 100 {
		files = files[:100]
	}
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
	if hash != "" {
		fmt.Fprintf(&buf, "- Commit %s: %s\n", hash, msg)
	}
	fmt.Fprintf(&buf, "- Generated Scheme for %d/%d programs\n", compiled, total)
	buf.WriteString("- Updated README checklist and outputs\n\n")
	if data, err := os.ReadFile(taskFile); err == nil {
		parts := bytes.Split(data, []byte("## Progress"))
		if len(parts) > 1 {
			// Reattach the prefix before the first split section
			rest := parts[1:]
			if len(rest) > 9 {
				rest = rest[:9]
			}
			for _, p := range rest {
				buf.WriteString("## Progress")
				buf.Write(p)
			}
		}
	}
	_ = os.WriteFile(taskFile, buf.Bytes(), 0o644)
}
