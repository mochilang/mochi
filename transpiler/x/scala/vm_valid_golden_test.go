//go:build slow

package scalat_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/compiler/x/testutil"
	"mochi/golden"
	"mochi/parser"
	scalat "mochi/transpiler/x/scala"
	"mochi/types"
)

func ensureScala(t *testing.T) {
	if _, err := exec.LookPath("scalac"); err != nil {
		t.Skip("scalac not installed")
	}
	if _, err := exec.LookPath("scala"); err != nil {
		t.Skip("scala not installed")
	}
}

func TestScalaTranspiler_VMValid_Golden(t *testing.T) {
	ensureScala(t)
	root := testutil.FindRepoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "scala")
	os.MkdirAll(outDir, 0o755)

	golden.RunWithSummary(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, base+".scala")
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
		ast, err := scalat.Transpile(prog, env)
		if err != nil {
			_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
			return nil, err
		}
		code := scalat.Emit(ast)
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return nil, err
		}
		tmp := t.TempDir()
		if out, err := exec.Command("scalac", "-d", tmp, codePath).CombinedOutput(); err != nil {
			_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
			return nil, err
		}
		cmd := exec.Command("scala", "-cp", tmp, "Main")
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
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".scala")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, "- "+mark+" "+name)
	}
	var buf bytes.Buffer
	buf.WriteString("# Scala Transpiler Output\n\n")
	buf.WriteString("Generated Scala code for programs in `tests/vm/valid`. Each program has a `.scala` file produced by the transpiler and a `.out` file with its runtime output. Compilation or execution errors are captured in `.error` files.\n\n")
	buf.WriteString(fmt.Sprintf("## Golden Test Checklist (%d/%d)\n\n", compiled, total))
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(filepath.Join(readmeDir, "README.md"), buf.Bytes(), 0o644)
}

func updateTasks() {
	root := repoRoot()
	taskFile := filepath.Join(root, "transpiler", "x", "scala", "TASKS.md")
	out, err := exec.Command("git", "log", "-1", "--format=%cd", "--date=format:%Y-%m-%d %H:%M:%S %Z").Output()
	ts := strings.TrimSpace(string(out))
	if err != nil {
		ts = ""
	}
	var buf bytes.Buffer
	buf.WriteString(fmt.Sprintf("## Progress (%s)\n", ts))
	buf.WriteString("- VM valid golden test results updated\n\n")
	if data, err := os.ReadFile(taskFile); err == nil {
		buf.Write(data)
	}
	_ = os.WriteFile(taskFile, buf.Bytes(), 0o644)
}
