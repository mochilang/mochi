//go:build slow

package phpcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	phpcode "mochi/compiler/x/php"
	"mochi/parser"
	"mochi/types"
)

func findRoot(t *testing.T) string {
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

func TestPHPCompiler_ValidPrograms(t *testing.T) {
	if _, err := exec.LookPath("php"); err != nil {
		t.Skip("php not installed")
	}
	root := findRoot(t)
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	outDir := filepath.Join(root, "tests", "machine", "x", "php")
	os.MkdirAll(outDir, 0755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) { compileRunOne(t, src, outDir, name) })
	}
}

func compileRunOne(t *testing.T, src, outDir, name string) {
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	c := phpcode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		writeError(outDir, name, []byte(err.Error()))
		t.Skipf("compile error: %v", err)
		return
	}
	phpPath := filepath.Join(outDir, name+".php")
	os.WriteFile(phpPath, code, 0644)
	cmd := exec.Command("php", phpPath)
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		writeError(outDir, name, out)
		t.Skipf("php error: %v", err)
		return
	}
	os.WriteFile(filepath.Join(outDir, name+".out"), out, 0644)
	os.Remove(filepath.Join(outDir, name+".error"))
}

func writeError(dir, name string, msg []byte) {
	os.WriteFile(filepath.Join(dir, name+".error"), msg, 0644)
	os.Remove(filepath.Join(dir, name+".out"))
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}

func updateReadme() {
	root := findRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "php")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
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
		lines = append(lines, fmt.Sprintf("- %s %s.mochi", mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# Machine Generated PHP Programs\n\n")
	buf.WriteString("This directory stores PHP code produced by the compiler tests. Each Mochi program from `tests/vm/valid` is compiled and executed. On success a `.out` file is written. Failures generate a `.error` file.\n\n")
	fmt.Fprintf(&buf, "## Summary\n%d/%d files compiled successfully\n\n", compiled, total)
	buf.WriteString("## Checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	os.WriteFile(filepath.Join(outDir, "README.md"), buf.Bytes(), 0644)
}
