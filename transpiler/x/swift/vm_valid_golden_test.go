//go:build slow

package swifttrans_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"mochi/compiler/x/testutil"
	"mochi/golden"
	"mochi/parser"
	swifttrans "mochi/transpiler/x/swift"
	"mochi/types"
)

func TestSwiftTranspiler_VMValid_Golden(t *testing.T) {
	swiftExe := ensureSwift(t)
	root := testutil.FindRepoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "swift")
	os.MkdirAll(outDir, 0o755)

	golden.RunWithSummary(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, base+".swift")
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
		ast, err := swifttrans.Transpile(env, prog, false)
		if err != nil {
			_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
			return nil, err
		}
		code := ast.Emit()
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return nil, err
		}
		out, err := compileAndRunSwiftSrc(t, swiftExe, code, nil, false)
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
	updateRosetta()
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
	binDir := filepath.Join(root, "tests", "transpiler", "x", "swift")
	readmePath := filepath.Join(root, "transpiler", "x", "swift", "README.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(binDir, name+".swift")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, "- "+mark+" "+name)
	}
	tsRaw, _ := exec.Command("git", "log", "-1", "--format=%cI").Output()
	tsStr := strings.TrimSpace(string(tsRaw))
	if t, err := time.Parse(time.RFC3339, tsStr); err == nil {
		if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
			tsStr = t.In(loc).Format("2006-01-02 15:04 -0700")
		} else {
			tsStr = t.Format("2006-01-02 15:04 MST")
		}
	} else {
		tsStr = time.Now().Format("2006-01-02 15:04 MST")
	}

	var buf bytes.Buffer
	buf.WriteString("# Swift Transpiler Output\n\n")
	buf.WriteString("Generated Swift code for programs in `tests/vm/valid`. Each program has a `.swift` file produced by the transpiler and a `.out` file with its runtime output. Compilation or execution errors are captured in `.error` files.\n\n")
	buf.WriteString(fmt.Sprintf("Transpiled programs: %d/%d\n", compiled, total))
	buf.WriteString(fmt.Sprintf("Last updated: %s\n\n", tsStr))
	buf.WriteString("Checklist:\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func updateTasks() {
	root := repoRoot()
	taskFile := filepath.Join(root, "transpiler", "x", "swift", "TASKS.md")
	tsRaw, _ := exec.Command("git", "log", "-1", "--format=%cI").Output()
	msgRaw, _ := exec.Command("git", "log", "-1", "--format=%s").Output()
	tsStr := strings.TrimSpace(string(tsRaw))
	if t, err := time.Parse(time.RFC3339, tsStr); err == nil {
		if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
			tsStr = t.In(loc).Format("2006-01-02 15:04 -0700")
		} else {
			tsStr = t.Format("2006-01-02 15:04 MST")
		}
	} else {
		tsStr = time.Now().Format("2006-01-02 15:04 MST")
	}
	msg := strings.TrimSpace(string(msgRaw))

	readmePath := filepath.Join(root, "transpiler", "x", "swift", "README.md")
	data, _ := os.ReadFile(readmePath)
	compiled := bytes.Count(data, []byte("[x]"))
	total := bytes.Count(data, []byte("- ["))

	var buf bytes.Buffer
	buf.WriteString(fmt.Sprintf("## Progress (%s)\n", tsStr))
	if msg != "" {
		buf.WriteString(fmt.Sprintf("- %s\n", msg))
	}
	buf.WriteString(fmt.Sprintf("- Generated golden tests for %d/%d programs\n", compiled, total))
	buf.WriteString("- Updated README checklist and output artifacts\n\n")

	if prev, err := os.ReadFile(taskFile); err == nil {
		sections := bytes.Split(prev, []byte("\n\n"))
		for _, sec := range sections {
			sec = bytes.TrimSpace(sec)
			if len(sec) == 0 || bytes.HasPrefix(sec, []byte("## Progress")) {
				continue
			}
			buf.Write(sec)
			buf.Write([]byte("\n\n"))
		}
	}
	_ = os.WriteFile(taskFile, buf.Bytes(), 0o644)
}
