//go:build slow

package phpcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"testing"

	phpcode "mochi/compiler/x/php"
	"mochi/parser"
	"mochi/types"
)

func TestCompileValidPrograms(t *testing.T) {
	if err := phpcode.EnsurePHP(); err != nil {
		t.Skipf("php not installed: %v", err)
	}

	root := findRepoRoot(t)
	outDir := filepath.Join(root, "tests", "machine", "x", "php")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}

	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob: %v", err)
	}

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) { compileOne(t, src, outDir, name) })
	}
}

func compileOne(t *testing.T, src, outDir, name string) {
	data, err := os.ReadFile(src)
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	prog, err := parser.Parse(src)
	if err != nil {
		writeError(outDir, name, data, err)
		return
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeError(outDir, name, data, errs[0])
		return
	}
	c := phpcode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		writeError(outDir, name, data, err)
		return
	}

	phpPath := filepath.Join(outDir, name+".php")
	if err := os.WriteFile(phpPath, code, 0644); err != nil {
		t.Fatalf("write php: %v", err)
	}

	cmd := exec.Command("php", phpPath)
	if inData, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(inData)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		writeError(outDir, name, data, fmt.Errorf("run error: %v\n%s", err, out))
		return
	}
	outFile := filepath.Join(outDir, name+".out")
	if err := os.WriteFile(outFile, out, 0644); err != nil {
		t.Fatalf("write out: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, name+".error"))
}

func writeError(dir, name string, src []byte, err error) {
	line := extractLine(err.Error())
	var context string
	if line > 0 {
		lines := bytes.Split(src, []byte("\n"))
		start := line - 2
		if start < 1 {
			start = 1
		}
		end := line + 2
		if end > len(lines) {
			end = len(lines)
		}
		var b strings.Builder
		for i := start; i <= end; i++ {
			b.WriteString(fmt.Sprintf("%4d: %s\n", i, lines[i-1]))
		}
		context = b.String()
	}
	msg := fmt.Sprintf("line: %d\nerror: %v\n%s", line, err, context)
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(msg), 0644)
}

func extractLine(msg string) int {
	re := regexp.MustCompile(`:(\d+):`)
	if m := re.FindStringSubmatch(msg); m != nil {
		if n, err := strconv.Atoi(m[1]); err == nil {
			return n
		}
	}
	re = regexp.MustCompile(`line (\d+)`)
	if m := re.FindStringSubmatch(msg); m != nil {
		if n, err := strconv.Atoi(m[1]); err == nil {
			return n
		}
	}
	return 0
}

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
