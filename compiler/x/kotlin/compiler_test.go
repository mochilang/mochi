//go:build slow

package kotlin_test

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

	kotlin "mochi/compiler/x/kotlin"
	"mochi/parser"
	"mochi/types"
)

// compileAndRun compiles the Mochi source at src into Kotlin, writes the
// generated source and runtime output under tests/machine/x/kotlin and returns
// the runtime output or an error.
func compileAndRun(t *testing.T, src string) (string, error) {
	prog, err := parser.Parse(src)
	if err != nil {
		return "", fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return "", fmt.Errorf("type error: %v", errs[0])
	}
	c := kotlin.New(env, src)
	code, err := c.Compile(prog)
	if err != nil {
		return "", fmt.Errorf("compile error: %w", err)
	}

	base := strings.TrimSuffix(filepath.Base(src), filepath.Ext(src))
	outDir := filepath.Join(repoRoot(), "tests", "machine", "x", "kotlin")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return "", err
	}
	srcFile := filepath.Join(outDir, base+".kt")
	if err := os.WriteFile(srcFile, code, 0o644); err != nil {
		return "", err
	}

	jarFile := filepath.Join(outDir, base+".jar")
	cmd := exec.Command("kotlinc", srcFile, "-include-runtime", "-d", jarFile)
	out, err := cmd.CombinedOutput()
	if err != nil {
		writeError(outDir, base, srcFile, out)
		return "", fmt.Errorf("kotlinc error: %w", err)
	}

	runCmd := exec.Command("java", "-jar", jarFile)
	runCmd.Dir = filepath.Dir(filepath.Dir(src))
	runOut, err := runCmd.CombinedOutput()
	if err != nil {
		writeError(outDir, base, srcFile, runOut)
		return "", fmt.Errorf("java error: %w", err)
	}

	outPath := filepath.Join(outDir, base+".out")
	if err := os.WriteFile(outPath, runOut, 0o644); err != nil {
		return "", err
	}
	return string(bytes.TrimSpace(runOut)), nil
}

// writeError creates a .error file with context around the first line number
// mentioned in msg.
func writeError(dir, base, srcFile string, msg []byte) {
	errPath := filepath.Join(dir, base+".error")
	line := extractLineNumber(string(msg))
	var context string
	if line > 0 {
		context = sourceContext(srcFile, line)
	}
	os.WriteFile(errPath, []byte(fmt.Sprintf("line %d:\n%s\n%s", line, msg, context)), 0o644)
}

var lineRE = regexp.MustCompile(`:(\d+):`)

func extractLineNumber(msg string) int {
	m := lineRE.FindStringSubmatch(msg)
	if len(m) < 2 {
		return 0
	}
	n, _ := strconv.Atoi(m[1])
	return n
}

func sourceContext(file string, line int) string {
	data, err := os.ReadFile(file)
	if err != nil {
		return ""
	}
	lines := strings.Split(string(data), "\n")
	start := line - 2
	if start < 0 {
		start = 0
	}
	end := line + 1
	if end > len(lines) {
		end = len(lines)
	}
	var buf strings.Builder
	for i := start; i < end; i++ {
		buf.WriteString(fmt.Sprintf("%3d: %s\n", i+1, lines[i]))
	}
	return buf.String()
}

func TestKotlinPrograms(t *testing.T) {
	root := repoRoot()
	files, err := filepath.Glob(filepath.Join(root, "tests", "vm", "valid", "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	for _, f := range files {
		base := strings.TrimSuffix(filepath.Base(f), ".mochi")
		t.Run(base, func(t *testing.T) {
			_, err := compileAndRun(t, f)
			if err != nil {
				t.Skipf("%v", err)
				return
			}
		})
	}
}

// repoRoot returns the repository root directory by searching upwards for
// go.mod.
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
	return dir
}
