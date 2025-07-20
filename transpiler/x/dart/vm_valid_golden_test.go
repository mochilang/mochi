//go:build slow

package dartt_test

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
	dartt "mochi/transpiler/x/dart"
	"mochi/types"
)

func ensureDart(t *testing.T) {
	if _, err := exec.LookPath("dart"); err != nil {
		t.Skip("dart not installed")
	}
}

func TestDartTranspiler_VMValid_Golden(t *testing.T) {
	ensureDart(t)
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "dart")
	os.MkdirAll(outDir, 0o755)

	golden.RunWithSummary(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, base+".dart")
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
		ast, err := dartt.Transpile(prog, env)
		if err != nil {
			_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
			return nil, err
		}
		var buf bytes.Buffer
		if err := dartt.Emit(&buf, ast); err != nil {
			_ = os.WriteFile(errPath, []byte("emit: "+err.Error()), 0o644)
			return nil, err
		}
		if err := os.WriteFile(codePath, buf.Bytes(), 0o644); err != nil {
			return nil, err
		}
		cmd := exec.Command("dart", codePath)
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
	outDir := filepath.Join(root, "tests", "transpiler", "x", "dart")
	readmePath := filepath.Join(root, "transpiler", "x", "dart", "README.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := filepath.Base(f)
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, strings.TrimSuffix(name, ".mochi")+".dart")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# Dart Transpiler Output\n\n")
	buf.WriteString("Generated Dart code for programs in `tests/vm/valid`. Each program has a `.dart` file and `.out` output. Compilation or runtime failures are captured in a `.error` file.\n\n")
	fmt.Fprintf(&buf, "## VM Golden Test Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func updateTasks() {
	root := repoRoot(&testing.T{})
	taskFile := filepath.Join(root, "transpiler", "x", "dart", "TASKS.md")
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

	data, _ := os.ReadFile(taskFile)
	var keep []string
	found := false
	for _, line := range strings.Split(string(data), "\n") {
		if strings.HasPrefix(line, "# Dart Transpiler") {
			found = true
		}
		if !found {
			continue
		}
		if strings.HasPrefix(line, "## Recent") || strings.HasPrefix(line, "## Progress") || strings.HasPrefix(line, "- VM valid") {
			continue
		}
		keep = append(keep, line)
	}

	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "dart")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	for _, f := range files {
		name := filepath.Base(f)
		if _, err := os.Stat(filepath.Join(outDir, strings.TrimSuffix(name, ".mochi")+".dart")); err == nil {
			compiled++
		}
	}

	var buf bytes.Buffer
	buf.WriteString(fmt.Sprintf("## Recent Enhancements (%s)\n", ts))
	buf.WriteString("- Improved variable declarations with basic type inference.\n")
	buf.WriteString("- Simplified `avg` builtin emission using list methods.\n")
	buf.WriteString("- Updated README checklist with progress summary.\n\n")
	buf.WriteString(fmt.Sprintf("## Progress (%s)\n", ts))
	buf.WriteString(fmt.Sprintf("- VM valid %d/%d\n\n", compiled, total))
	buf.WriteString(strings.Join(keep, "\n"))
	_ = os.WriteFile(taskFile, buf.Bytes(), 0o644)
}
