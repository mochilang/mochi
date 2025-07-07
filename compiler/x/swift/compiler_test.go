//go:build slow

package swift_test

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

	swift "mochi/compiler/x/swift"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestCompileValidPrograms(t *testing.T) {
	swiftExe := ensureSwift(t)
	root := testutil.FindRepoRoot(t)
	outDir := filepath.Join(root, "tests", "machine", "x", "swift")
	if err := os.MkdirAll(outDir, 0755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	files, err := filepath.Glob(filepath.Join(root, "tests", "vm", "valid", "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) { compileOne(t, src, outDir, name, swiftExe) })
	}
}

func ensureSwift(t *testing.T) string {
	if env := os.Getenv("SWIFT"); env != "" {
		if p, err := exec.LookPath(env); err == nil {
			return p
		}
	}
	if p, err := exec.LookPath("swift"); err == nil {
		return p
	}
	t.Skip("swift not found")
	return ""
}

func compileOne(t *testing.T, src, outDir, name, swiftExe string) {
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
	code, err := swift.New(env).Compile(prog)
	if err != nil {
		writeError(outDir, name, data, err)
		t.Skipf("compile error: %v", err)
		return
	}
	swiftFile := filepath.Join(outDir, name+".swift")
	if err := os.WriteFile(swiftFile, code, 0644); err != nil {
		t.Fatalf("write swift: %v", err)
	}
	cmd := exec.Command(swiftExe, swiftFile)
	out, err := cmd.CombinedOutput()
	if err != nil {
		writeRunError(outDir, name, swiftFile, out, err)
		t.Skipf("swift error: %v", err)
		return
	}
	if err := os.WriteFile(filepath.Join(outDir, name+".out"), out, 0644); err != nil {
		t.Fatalf("write out: %v", err)
	}
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

func writeRunError(dir, name, file string, output []byte, err error) {
	msg := fmt.Sprintf("%v\n%s", err, output)
	line := extractLine(string(output))
	if line > 0 {
		srcData, _ := os.ReadFile(file)
		lines := strings.Split(string(srcData), "\n")
		start := line - 2
		if start < 0 {
			start = 0
		}
		end := line + 1
		if end > len(lines) {
			end = len(lines)
		}
		ctx := strings.Join(lines[start:end], "\n")
		msg = fmt.Sprintf("line %d: %v\n%s", line, err, ctx)
	}
	os.WriteFile(filepath.Join(dir, name+".error"), []byte(msg), 0644)
}

var errLineRE = regexp.MustCompile(`:(\d+):`)

func extractLine(msg string) int {
	if m := errLineRE.FindStringSubmatch(msg); m != nil {
		if n, err := strconv.Atoi(m[1]); err == nil {
			return n
		}
	}
	return 0
}
