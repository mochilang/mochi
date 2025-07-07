//go:build slow

package ftncode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	ftncode "mochi/compiler/x/fortran"
)

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

func TestFortranCompiler_Programs(t *testing.T) {
	gfortran, err := ftncode.EnsureFortran()
	if err != nil {
		t.Skipf("gfortran not installed: %v", err)
	}
	root := repoRoot(t)
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	outDir := filepath.Join(root, "tests", "machine", "x", "fortran")
	os.MkdirAll(outDir, 0o755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), filepath.Ext(src))
		t.Run(name, func(t *testing.T) {
			code, err := ftncode.New().CompileFile(src)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			ffile := filepath.Join(outDir, name+".f90")
			if err := os.WriteFile(ffile, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			exe := filepath.Join(outDir, name)
			if out, err := exec.Command(gfortran, "-ffree-line-length-none", ffile, "-o", exe).CombinedOutput(); err != nil {
				line := extractLineNumber(string(out))
				ctx := contextLines(ffile, line, 2)
				msg := fmt.Sprintf("line %d: %v\n%s\n%s", line, err, string(out), ctx)
				os.WriteFile(filepath.Join(outDir, name+".error"), []byte(msg), 0644)
				t.Fatalf("gfortran error: %v", err)
			}
			cmd := exec.Command(exe)
			var buf bytes.Buffer
			cmd.Stdout = &buf
			cmd.Stderr = &buf
			if err := cmd.Run(); err != nil {
				line := extractLineNumber(buf.String())
				ctx := contextLines(ffile, line, 2)
				msg := fmt.Sprintf("line %d: %v\n%s\n%s", line, err, buf.String(), ctx)
				os.WriteFile(filepath.Join(outDir, name+".error"), []byte(msg), 0644)
				t.Fatalf("run error: %v", err)
			}
			norm := normalize(buf.Bytes())
			os.WriteFile(filepath.Join(outDir, name+".out"), norm, 0644)
		})
	}
}

func extractLineNumber(out string) int {
	parts := strings.Split(out, ":")
	for i := 0; i < len(parts)-1; i++ {
		if n, err := strconv.Atoi(parts[i+1]); err == nil {
			return n
		}
	}
	return 0
}

func contextLines(file string, line, n int) string {
	data, err := os.ReadFile(file)
	if err != nil || line <= 0 {
		return ""
	}
	lines := strings.Split(string(data), "\n")
	start := line - n - 1
	if start < 0 {
		start = 0
	}
	end := line + n
	if end > len(lines) {
		end = len(lines)
	}
	var b strings.Builder
	for i := start; i < end; i++ {
		fmt.Fprintf(&b, "%4d: %s\n", i+1, lines[i])
	}
	return b.String()
}

func normalize(out []byte) []byte {
	s := strings.TrimSpace(string(out))
	lines := strings.Split(s, "\n")
	for i, ln := range lines {
		ln = strings.TrimSpace(ln)
		for strings.Contains(ln, "  ") {
			ln = strings.ReplaceAll(ln, "  ", " ")
		}
		switch ln {
		case "T", "TRUE", ".TRUE.":
			ln = "true"
		case "F", "FALSE", ".FALSE.":
			ln = "false"
		}
		lines[i] = ln
	}
	return []byte(strings.Join(lines, "\n"))
}
