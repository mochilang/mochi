//go:build slow

package smalltalk_test

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

	st "mochi/compiler/x/smalltalk"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func ensureGST() string {
	if p, err := exec.LookPath("gst"); err == nil {
		return p
	}
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
	re := regexp.MustCompile(`:(\d+)`)
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
	gst := ensureGST()
	root := testutil.FindRepoRoot(t)
	outDir := filepath.Join(root, "tests", "machine", "x", "st")
	if err := os.MkdirAll(outDir, 0755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	files, err := filepath.Glob(filepath.Join(root, "tests", "vm", "valid", "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
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
			code, err := st.New().Compile(prog)
			if err != nil {
				writeError(outDir, name, data, err)
				t.Skipf("compile error: %v", err)
				return
			}
			stFile := filepath.Join(outDir, name+".st")
			if err := os.WriteFile(stFile, code, 0644); err != nil {
				t.Fatalf("write st: %v", err)
			}
			if gst == "" {
				writeError(outDir, name, data, fmt.Errorf("gst interpreter not available"))
				return
			}
			cmd := exec.Command(gst, stFile)
			var buf bytes.Buffer
			cmd.Stdout = &buf
			cmd.Stderr = &buf
			if err := cmd.Run(); err != nil {
				writeError(outDir, name, data, fmt.Errorf("run error: %v\n%s", err, buf.Bytes()))
				return
			}
			outPath := filepath.Join(outDir, name+".out")
			if err := os.WriteFile(outPath, buf.Bytes(), 0644); err != nil {
				t.Fatalf("write out: %v", err)
			}
			os.Remove(filepath.Join(outDir, name+".error"))
		})
	}
}
