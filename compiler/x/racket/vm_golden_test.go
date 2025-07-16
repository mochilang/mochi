//go:build slow

package racket_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	rack "mochi/compiler/x/racket"
	testutil "mochi/compiler/x/testutil"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func writeError(path string, src []byte, err error) {
	var buf bytes.Buffer
	fmt.Fprintf(&buf, "%v\n", err)
	buf.Write(src)
	_ = os.WriteFile(path, buf.Bytes(), 0o644)
}

// TestRacketCompiler_VMValid_Golden compiles programs under tests/vm/valid to
// Racket, runs them and compares output against golden files in
// tests/machine/x/racket. Generated code is also written to that directory.
func TestRacketCompiler_VMValid_Golden(t *testing.T) {
	if err := rack.EnsureRacket(); err != nil {
		t.Skipf("racket not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	outDir := filepath.Join(root, "tests", "machine", "x", "racket")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatal(err)
	}

	golden.RunWithSummary(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		data, err := os.ReadFile(src)
		if err != nil {
			return nil, err
		}
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, name+".rkt")
		outPath := filepath.Join(outDir, name+".out")
		errPath := filepath.Join(outDir, name+".error")

		prog, err := parser.Parse(src)
		if err != nil {
			writeError(errPath, data, err)
			return nil, err
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			writeError(errPath, data, errs[0])
			return nil, errs[0]
		}
		code, err := rack.New().Compile(prog)
		if err != nil {
			writeError(errPath, data, err)
			return nil, err
		}
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return nil, err
		}
		cmd := exec.Command("racket", codePath)
		if inData, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(inData)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			writeError(errPath, code, fmt.Errorf("run error: %w\n%s", err, out))
			return nil, err
		}
		out = bytes.TrimSpace(out)
		if err := os.WriteFile(outPath, out, 0o644); err != nil {
			return nil, err
		}
		os.Remove(errPath)
		return out, nil
	})
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}

func updateReadme() {
	root := testutil.FindRepoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "racket")
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
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# Racket Machine Output\n\n")
	buf.WriteString("This directory contains Racket source code generated from the Mochi programs in `tests/vm/valid` using the Racket backend. Each program was compiled and executed. Successful runs produced an `.out` file while failures produced an `.error` file.\n\n")
	fmt.Fprintf(&buf, "Compiled programs: %d/%d\n\n", compiled, total)
	buf.WriteString("## Checklist\n\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(filepath.Join(outDir, "README.md"), buf.Bytes(), 0o644)
}
