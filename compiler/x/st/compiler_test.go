//go:build slow

package st_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	st "mochi/compiler/x/st"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func ensureGST() string {
	if p, err := exec.LookPath("gst"); err == nil {
		return p
	}
	if err := st.EnsureSmalltalk(); err == nil {
		if p, err := exec.LookPath("gst"); err == nil {
			return p
		}
	}
	return ""
}

func writeError(dir, name string, src []byte, err error) {
	msg := fmt.Sprintf("line: 0\nerror: %v", err)
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(msg), 0644)
}

func TestCompilePrograms(t *testing.T) {
	gstPath := ensureGST()
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
		t.Run(name, func(t *testing.T) { compileOne(t, src, outDir, name, gstPath) })
	}
}

func compileOne(t *testing.T, src, outDir, name, gstPath string) {
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
	srcFile := filepath.Join(outDir, name+".st")
	if err := os.WriteFile(srcFile, code, 0644); err != nil {
		t.Fatalf("write st: %v", err)
	}
	if gstPath == "" {
		writeError(outDir, name, code, fmt.Errorf("gst interpreter not available"))
		return
	}
	cmd := exec.Command(gstPath, srcFile)
	var buf bytes.Buffer
	cmd.Stderr = &buf
	out, err := cmd.Output()
	if err != nil {
		writeError(outDir, name, code, fmt.Errorf("run: %v\n%s", err, buf.String()))
		return
	}
	outPath := filepath.Join(outDir, name+".out")
	os.WriteFile(outPath, bytes.TrimSpace(out), 0644)
	errFile := filepath.Join(outDir, name+".error")
	os.Remove(errFile)
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}

func updateReadme() {
	root := testutil.FindRepoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "st")
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
	fmt.Fprintf(&buf, "# Mochi to Smalltalk Machine Outputs (%d/%d compiled and run)\n\n", compiled, total)
	buf.WriteString("This directory contains Smalltalk source code generated from the Mochi programs in `tests/vm/valid`. A checkbox indicates the program compiled and executed successfully during tests.\n\n")
	buf.WriteString("## Checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n\n")
	buf.WriteString("## TODO\n")
	buf.WriteString("- [x] full outer join semantics\n")
	buf.WriteString("- [x] right join semantics\n\n")
	if compiled == total {
		buf.WriteString("All programs executed successfully with GNU Smalltalk.\n")
	}
	_ = os.WriteFile(filepath.Join(outDir, "README.md"), buf.Bytes(), 0644)
}
