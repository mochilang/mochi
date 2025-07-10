//go:build slow

package dart_test

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

	dart "mochi/compiler/x/dart"
	"mochi/parser"
	"mochi/types"
)

func TestDartCompiler_ValidPrograms(t *testing.T) {
	root := findRepoRoot(t)
	srcPattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(srcPattern)
	if err != nil {
		t.Fatalf("list sources: %v", err)
	}
	outDir := filepath.Join(root, "tests", "machine", "x", "dart")
	os.MkdirAll(outDir, 0755)

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				writeError(outDir, name, src, fmt.Errorf("parse error: %w", err))
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				writeError(outDir, name, src, fmt.Errorf("type error: %v", errs[0]))
				return
			}
			code, err := dart.New(env).Compile(prog)
			if err != nil {
				writeError(outDir, name, src, fmt.Errorf("compile error: %w", err))
				return
			}
			srcFile := filepath.Join(outDir, name+".dart")
			if err := os.WriteFile(srcFile, code, 0644); err != nil {
				writeError(outDir, name, src, fmt.Errorf("write error: %w", err))
				return
			}
			cmd := exec.Command("dart", srcFile)
			out, err := cmd.CombinedOutput()
			if err != nil {
				writeCompileError(outDir, name, srcFile, out, err)
				return
			}
			os.WriteFile(filepath.Join(outDir, name+".out"), bytes.TrimSpace(out), 0644)
			os.Remove(filepath.Join(outDir, name+".error"))
		})
	}
}

func TestDartCompiler_TPCHQueries(t *testing.T) {
	if _, err := exec.LookPath("dart"); err != nil {
		t.Skip("dart not installed")
	}
	root := findRepoRoot(t)
	for i := 1; i <= 22; i++ {
		q := fmt.Sprintf("q%d", i)
		codeWant := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "dart", q+".dart.out")
		outWant := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "dart", q+".out")
		if _, err := os.Stat(codeWant); err != nil {
			continue
		}
		t.Run(q, func(t *testing.T) {
			src := filepath.Join(root, "tests", "dataset", "tpc-h", q+".mochi")
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := dart.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			wantCode, err := os.ReadFile(codeWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q+".dart.out", got, bytes.TrimSpace(wantCode))
			}
			tmp := t.TempDir()
			file := filepath.Join(tmp, "prog.dart")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("dart", file)
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("dart run error: %v\n%s", err, out)
			}
			gotOut := bytes.TrimSpace(out)
			wantOut, err := os.ReadFile(outWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q+".out", gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}

func writeError(dir, name, src string, err error) {
	msg := err.Error()
	os.WriteFile(filepath.Join(dir, name+".error"), []byte(msg), 0644)
}

var errLineRE = regexp.MustCompile(`:(\d+):`) // extract line number

func writeCompileError(dir, name, file string, output []byte, err error) {
	msg := fmt.Sprintf("%v\n%s", err, output)
	line := 0
	if m := errLineRE.FindSubmatch(output); len(m) == 2 {
		line = atoi(string(m[1]))
	}
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
		if start > len(lines) {
			start = len(lines)
		}
		ctx := strings.Join(lines[start:end], "\n")
		msg = fmt.Sprintf("line %d: %v\n%s", line, err, ctx)
	}
	os.WriteFile(filepath.Join(dir, name+".error"), []byte(msg), 0644)
}

func atoi(s string) int {
	n, _ := strconv.Atoi(s)
	return n
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

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}

func updateReadme() {
	root := findRepoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "dart")
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
	buf.WriteString("# Dart Machine Output\n\n")
	buf.WriteString("This directory contains Dart code generated from Mochi programs. Successful runs have a .out file, failures a .error file.\n\n")
	fmt.Fprintf(&buf, "Compiled programs: %d/%d\n\n", compiled, total)
	buf.WriteString("Checklist:\n\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	os.WriteFile(filepath.Join(outDir, "README.md"), buf.Bytes(), 0644)
}
