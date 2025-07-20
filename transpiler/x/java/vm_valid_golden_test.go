//go:build slow

package javatr_test

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
	javatr "mochi/transpiler/x/java"
	"mochi/types"
)

func TestJavaTranspiler_VMValid_Golden(t *testing.T) {
	if _, err := exec.LookPath("javac"); err != nil {
		t.Skip("javac not installed")
	}
	if _, err := exec.LookPath("java"); err != nil {
		t.Skip("java runtime not installed")
	}
	root := repoRootDir(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "java")
	os.MkdirAll(outDir, 0o755)

	golden.RunWithSummary(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, base+".java")
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
		ast, err := javatr.Transpile(prog, env)
		if err != nil {
			_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
			return nil, err
		}
		code := javatr.Emit(ast)
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return nil, err
		}
		mainPath := filepath.Join(outDir, "Main.java")
		if err := os.WriteFile(mainPath, code, 0o644); err != nil {
			return nil, err
		}
		cmd := exec.Command("javac", "Main.java")
		cmd.Dir = outDir
		if out, err := cmd.CombinedOutput(); err != nil {
			_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
			return nil, err
		}
		cmd = exec.Command("java", "-cp", outDir, "Main")
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
	os.Exit(code)
}

func repoRootDir(t *testing.T) string {
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

func updateReadme() {
	root := repoRootDir(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "java")
	readmePath := filepath.Join(root, "transpiler", "x", "java", "README.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := filepath.Base(f)
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, strings.TrimSuffix(name, ".mochi")+".java")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# Java Transpiler Output\n\n")
	buf.WriteString("Generated Java code for programs in `tests/vm/valid`. Each program has a `.java` file produced by the transpiler and a `.out` file with its runtime output. Compilation or execution errors are captured in `.error` files.\n\n")
	fmt.Fprintf(&buf, "## VM Golden Test Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func updateTasks() {
	root := repoRootDir(&testing.T{})
	taskFile := filepath.Join(root, "transpiler", "x", "java", "TASKS.md")
	out, err := exec.Command("git", "log", "-1", "--format=%h%n%cI%n%s").Output()
	var hash, ts, msg string
	if err == nil {
		parts := strings.SplitN(strings.TrimSpace(string(out)), "\n", 3)
		if len(parts) == 3 {
			hash, ts, msg = parts[0], parts[1], parts[2]
		}
	}
	if t, perr := time.Parse(time.RFC3339, ts); perr == nil {
		if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
			ts = t.In(loc).Format("2006-01-02 15:04 -0700")
		} else {
			ts = t.Format("2006-01-02 15:04 MST")
		}
	}

	data, _ := os.ReadFile(taskFile)
	var keep []string
	for _, line := range strings.Split(string(data), "\n") {
		if strings.HasPrefix(line, "## Progress") || strings.HasPrefix(line, "- VM valid") {
			continue
		}
		keep = append(keep, line)
	}

	var buf bytes.Buffer
	buf.WriteString(fmt.Sprintf("## Progress (%s)\n", ts))
	if msg == "" {
		buf.WriteString("- VM valid golden test results updated\n\n")
	} else {
		buf.WriteString(fmt.Sprintf("- %s (%s)\n\n", msg, hash))
	}
	buf.WriteString(strings.Join(keep, "\n"))
	if len(keep) > 0 && keep[len(keep)-1] != "" {
		buf.WriteString("\n")
	}
	_ = os.WriteFile(taskFile, buf.Bytes(), 0o644)
}
