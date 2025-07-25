//go:build slow

package fortran_test

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
	ftn "mochi/transpiler/x/fortran"
	"mochi/types"
)

func TestFortranTranspiler_VMValid_Golden(t *testing.T) {
	if _, err := exec.LookPath("gfortran"); err != nil {
		t.Skip("gfortran not installed")
	}
	root := repoRootDir(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "fortran")
	os.MkdirAll(outDir, 0o755)

	golden.RunWithSummary(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		f90Path := filepath.Join(outDir, base+".f90")
		outPath := filepath.Join(outDir, base+".out")
		errPath := filepath.Join(outDir, base+".error")

		prog, err := parser.Parse(src)
		if err != nil {
			_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
			return nil, err
		}
		env := types.NewEnv(nil)
		var typeErr error
		if errs := types.Check(prog, env); len(errs) > 0 {
			typeErr = errs[0]
		}
		bench := os.Getenv("MOCHI_BENCHMARK") != "" && os.Getenv("MOCHI_BENCHMARK") != "0"
		ftn.SetBenchMain(bench)
		ast, err := ftn.Transpile(prog, env)
		_ = typeErr
		if err != nil {
			_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
			return nil, err
		}
		code := ast.Emit()
		if err := os.WriteFile(f90Path, code, 0o644); err != nil {
			return nil, err
		}
		exe := filepath.Join(outDir, base)
		if out, err := exec.Command("gfortran", f90Path, "-o", exe).CombinedOutput(); err != nil {
			_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
			return nil, err
		}
		cmd := exec.Command(exe)
		if bench {
			cmd.Env = append(os.Environ(), "MOCHI_BENCHMARK=1")
		}
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
			return nil, err
		}
		outBytes := bytes.TrimSpace(out)
		benchPath := filepath.Join(outDir, base+".bench")
		if bench {
			benchData := outBytes
			if idx := bytes.LastIndex(benchData, []byte("{")); idx >= 0 {
				benchData = benchData[idx:]
			}
			_ = os.WriteFile(benchPath, benchData, 0o644)
			_ = os.Remove(outPath)
		} else {
			_ = os.WriteFile(outPath, outBytes, 0o644)
			_ = os.Remove(benchPath)
		}
		_ = os.Remove(errPath)
		return outBytes, nil
	})
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	updateChecklist()
	updateTasks()
	os.Exit(code)
}

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

func updateReadme() {
	root := repoRootDir(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	binDir := filepath.Join(root, "tests", "transpiler", "x", "fortran")
	readmePath := filepath.Join(root, "transpiler", "x", "fortran", "README.md")
	tsOut, _ := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := ""
	if t, err := time.Parse(time.RFC3339, strings.TrimSpace(string(tsOut))); err == nil {
		if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
			ts = t.In(loc).Format("2006-01-02 15:04:05 -0700")
		} else {
			ts = t.Format("2006-01-02 15:04:05 -0700")
		}
	}
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
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
	var buf bytes.Buffer
	buf.WriteString("# Transpiler Progress\n\n")
	buf.WriteString("This checklist tracks Mochi programs from `tests/vm/valid` that successfully transpile using the experimental Fortran backend.\n\n")
	fmt.Fprintf(&buf, "Checklist of programs that currently transpile and run (%d/%d):\n\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	if ts != "" {
		fmt.Fprintf(&buf, "\n_Last updated: %s_\n", ts)
	}
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func updateTasks() {
	root := repoRootDir(&testing.T{})
	taskFile := filepath.Join(root, "transpiler", "x", "fortran", "TASKS.md")
	tsOut, _ := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := ""
	if t, err := time.Parse(time.RFC3339, strings.TrimSpace(string(tsOut))); err == nil {
		if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
			ts = t.In(loc).Format("2006-01-02 15:04:05 -0700")
		} else {
			ts = t.Format("2006-01-02 15:04:05 -0700")
		}
	}
	msgOut, _ := exec.Command("git", "log", "-1", "--format=%s").Output()
	msg := strings.TrimSpace(string(msgOut))
	if msg == "" {
		msg = "update"
	}

	var buf bytes.Buffer
	buf.WriteString(fmt.Sprintf("## Progress (%s)\n", ts))
	buf.WriteString("- " + msg + "\n\n")
	if data, err := os.ReadFile(taskFile); err == nil {
		buf.Write(data)
	}
	_ = os.WriteFile(taskFile, buf.Bytes(), 0o644)
}
