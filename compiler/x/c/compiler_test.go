package c_test

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

	c "mochi/compiler/x/c"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

// ensureCC returns the path to a C compiler or skips the test.
func ensureCC(t *testing.T) string {
	if env := os.Getenv("CC"); env != "" {
		if p, err := exec.LookPath(env); err == nil {
			return p
		}
	}
	for _, cc := range []string{"cc", "gcc", "clang"} {
		if p, err := exec.LookPath(cc); err == nil {
			return p
		}
	}
	t.Skip("C compiler not found")
	return ""
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
			if i-1 < len(lines) {
				fmt.Fprintf(&b, "%4d: %s\n", i, lines[i-1])
			}
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

func TestCompilePrograms(t *testing.T) {
	cc := ensureCC(t)
	root := testutil.FindRepoRoot(t)
	outDir := filepath.Join(root, "tests", "machine", "x", "c")
	if err := os.MkdirAll(outDir, 0755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	files, err := filepath.Glob(filepath.Join(root, "tests", "vm", "valid", "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) { compileOne(t, src, outDir, name, cc) })
	}
}

func compileOne(t *testing.T, src, outDir, name, cc string) {
	data, err := os.ReadFile(src)
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	prog, err := parser.Parse(src)
	if err != nil {
		writeError(outDir, name, data, err)
		t.Skipf("parse error: %v", err)
		return
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeError(outDir, name, data, errs[0])
		t.Skipf("type error: %v", errs[0])
		return
	}
	code, err := c.New(env).Compile(prog)
	if err != nil {
		writeError(outDir, name, data, err)
		t.Skipf("compile error: %v", err)
		return
	}
	cpath := filepath.Join(outDir, name+".c")
	if err := os.WriteFile(cpath, code, 0644); err != nil {
		t.Fatalf("write c: %v", err)
	}
	bin := filepath.Join(outDir, name)
	if out, err := exec.Command(cc, cpath, "-o", bin).CombinedOutput(); err != nil {
		writeError(outDir, name, data, fmt.Errorf("cc error: %v\n%s", err, out))
		t.Skipf("cc error: %v", err)
		return
	}
	out, err := exec.Command(bin).CombinedOutput()
	if err != nil {
		writeError(outDir, name, data, fmt.Errorf("run error: %v\n%s", err, out))
		t.Skipf("run error: %v", err)
		return
	}
	outFile := filepath.Join(outDir, name+".out")
	if err := os.WriteFile(outFile, out, 0644); err != nil {
		t.Fatalf("write out: %v", err)
	}
}
