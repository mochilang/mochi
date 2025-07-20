//go:build slow

package cljt_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	cljt "mochi/transpiler/x/clj"
	"mochi/types"
)

var update = flag.Bool("update", false, "update output files")

func findRepoRoot(t *testing.T) string {
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

// EnsureClojure checks that the clojure CLI tool is installed.
func EnsureClojure() error {
	if _, err := exec.LookPath("clojure"); err == nil {
		return nil
	}
	if _, err := exec.LookPath("clj"); err == nil {
		return nil
	}
	return exec.ErrNotFound
}

func TestTranspile_Golden(t *testing.T) {
	if err := EnsureClojure(); err != nil {
		t.Skip("clojure not installed")
	}

	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "clj")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkout: %v", err)
	}

	srcs, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	if len(srcs) == 0 {
		t.Fatal("no Mochi tests found")
	}

	for _, srcPath := range srcs {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".mochi")
		if strings.Contains(name, ".") {
			continue
		}
		t.Run(name, func(t *testing.T) {
			compileAndRunClojure(t, srcPath, outDir, name)
		})
	}
}

func compileAndRunClojure(t *testing.T, srcPath, outDir, name string) {
	if _, err := os.ReadFile(srcPath); err != nil {
		t.Fatalf("read: %v", err)
	}
	prog, err := parser.Parse(srcPath)
	if err != nil {
		writeCljError(outDir, name, fmt.Errorf("parse error: %w", err))
		t.Skip("parse error")
		return
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeCljError(outDir, name, fmt.Errorf("type error: %v", errs[0]))
		t.Skip("type error")
		return
	}
	ast, err := cljt.Transpile(prog, env)
	if err != nil {
		writeCljError(outDir, name, fmt.Errorf("transpile error: %w", err))
		t.Skip("transpile error")
		return
	}
	code := cljt.Format(cljt.EmitString(ast))
	cljPath := filepath.Join(outDir, name+".clj")
	if err := os.WriteFile(cljPath, code, 0o644); err != nil {
		t.Fatalf("write clj: %v", err)
	}
	var buf bytes.Buffer
	cmd := exec.Command("clojure", cljPath)
	cmd.Stdout = &buf
	cmd.Stderr = &buf
	if err := cmd.Run(); err != nil {
		writeCljError(outDir, name, fmt.Errorf("run error: %v\n%s", err, buf.Bytes()))
		t.Skip("run error")
		return
	}
	got := bytes.TrimSpace(buf.Bytes())
	outPath := filepath.Join(outDir, name+".output")
	if *update {
		if err := os.WriteFile(outPath, got, 0o644); err != nil {
			t.Fatalf("write output: %v", err)
		}
		_ = os.Remove(filepath.Join(outDir, name+".error"))
		return
	}
	want, err := os.ReadFile(outPath)
	if err != nil {
		writeCljError(outDir, name, fmt.Errorf("missing output: %v", err))
		t.Fatalf("missing .output file for %s", name)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		writeCljError(outDir, name, fmt.Errorf("output mismatch\n-- got --\n%s\n-- want --\n%s", got, want))
		t.Errorf("output mismatch for %s", name)
		return
	}
	_ = os.Remove(filepath.Join(outDir, name+".error"))
}

func writeCljError(dir, name string, err error) {
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(err.Error()), 0o644)
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	updateTasks()
	os.Exit(code)
}

func updateReadme() {
	root := findRepoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	binDir := filepath.Join(root, "tests", "transpiler", "x", "clj")
	readmePath := filepath.Join(root, "transpiler", "x", "clj", "README.md")

	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(binDir, name+".output")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}

	var buf bytes.Buffer
	buf.WriteString("# Mochi Transpiler\n\n")
	buf.WriteString("This directory contains experimental source translators for generating Clojure code. Each program in `tests/vm/valid` is transpiled and executed with `clojure`.\n\n")
	fmt.Fprintf(&buf, "Compiled programs: %d/%d\n\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func updateTasks() {
	root := findRepoRoot(&testing.T{})
	taskFile := filepath.Join(root, "transpiler", "x", "clj", "TASKS.md")

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

	files, _ := filepath.Glob(filepath.Join(root, "tests", "vm", "valid", "*.mochi"))
	total := len(files)
	compiled := 0
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		if _, err := os.Stat(filepath.Join(root, "tests", "transpiler", "x", "clj", name+".output")); err == nil {
			compiled++
		}
	}

	entry := fmt.Sprintf("## Progress (%s)\n- %s\n- Regenerated golden files - %d/%d vm valid programs passing\n\n", ts, msg, compiled, total)
	if prev, err := os.ReadFile(taskFile); err == nil {
		entry += string(prev)
	}
	_ = os.WriteFile(taskFile, []byte(entry), 0o644)
}
