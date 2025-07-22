//go:build slow

package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"mochi/parser"
	scalat "mochi/transpiler/x/scala"
	"mochi/types"
)

func repoRoot() string {
	dir, _ := os.Getwd()
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
	return ""
}

func updateReadme() {
	root := repoRoot()
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "scala")
	readmeDir := filepath.Join(root, "transpiler", "x", "scala")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
	total := len(files)
	completed := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			completed++
			mark = "[x]"
		} else if _, err := os.Stat(filepath.Join(outDir, name+".error")); err == nil {
			mark = "[ ]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# Scala Transpiler Output\n\n")
	buf.WriteString("Generated Scala code for programs in `tests/vm/valid`. Each program has a `.scala` file produced by the transpiler and a `.out` file with its runtime output. Compilation or execution errors are captured in `.error` files.\n\n")
	loc, _ := time.LoadLocation("Asia/Bangkok")
	ts := time.Now().In(loc).Format("2006-01-02 15:04 -0700")
	buf.WriteString(fmt.Sprintf("## Golden Test Checklist (%d/%d)\n", completed, total))
	buf.WriteString(fmt.Sprintf("_Last updated: %s_\n\n", ts))
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(filepath.Join(readmeDir, "README.md"), buf.Bytes(), 0o644)
}

func updateTasks() {
	root := repoRoot()
	taskFile := filepath.Join(root, "transpiler", "x", "scala", "TASKS.md")
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
		if _, err := os.Stat(filepath.Join(root, "tests", "transpiler", "x", "scala", name+".out")); err == nil {
			compiled++
		}
	}
	entry := fmt.Sprintf("## Progress (%s)\n- %s\n- Regenerated golden files - %d/%d vm valid programs passing\n\n", ts, msg, compiled, total)
	if prev, err := os.ReadFile(taskFile); err == nil {
		entry += string(prev)
	}
	_ = os.WriteFile(taskFile, []byte(entry), 0o644)
}

func runCase(name string) error {
	root := repoRoot()
	src := filepath.Join(root, "tests", "vm", "valid", name+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		return fmt.Errorf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return fmt.Errorf("type: %v", errs[0])
	}
	ast, err := scalat.Transpile(prog, env)
	if err != nil {
		return fmt.Errorf("transpile: %v", err)
	}
	code := scalat.Emit(ast)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "scala")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return err
	}
	codePath := filepath.Join(outDir, name+".scala")
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		return err
	}
	tmp := filepath.Join(outDir, name+"_tmp")
	os.MkdirAll(tmp, 0o755)
	if out, err := exec.Command("scalac", "-d", tmp, codePath).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, name+".error"), append([]byte(err.Error()+"\n"), out...), 0o644)
		return fmt.Errorf("compile: %v", err)
	}
	cmd := exec.Command("scala", "-cp", tmp, "Main")
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		os.WriteFile(filepath.Join(outDir, name+".error"), append([]byte(err.Error()+"\n"), out...), 0o644)
		return fmt.Errorf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, name+".error"))
	if err := os.WriteFile(filepath.Join(outDir, name+".out"), got, 0o644); err != nil {
		return err
	}
	return nil
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "usage: gen_scala_golden <testname>...")
		os.Exit(1)
	}
	for _, name := range os.Args[1:] {
		if err := runCase(name); err != nil {
			fmt.Fprintf(os.Stderr, "%s: %v\n", name, err)
		}
	}
	updateReadme()
	updateTasks()
}
