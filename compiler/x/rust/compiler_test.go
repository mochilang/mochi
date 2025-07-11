//go:build slow

package rustcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	rustcode "mochi/compiler/x/rust"
	"mochi/parser"
	"mochi/types"
)

// findRepoRoot walks up directories until go.mod is found.
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

// writeError writes an error file with context around the failing line.
func writeError(dir, name, src string, err error) {
	lines := strings.Split(src, "\n")
	msg := err.Error()
	line := 0
	if idx := strings.Index(msg, ":"); idx != -1 {
		fmt.Sscanf(msg[idx+1:], "%d", &line)
	}
	start := line - 2
	if start < 0 {
		start = 0
	}
	end := line + 1
	if end > len(lines) {
		end = len(lines)
	}
	ctx := strings.Join(lines[start:end], "\n")
	os.WriteFile(filepath.Join(dir, name+".error"), []byte(fmt.Sprintf("line %d: %v\n%s", line, err, ctx)), 0644)
}

func TestCompilePrograms(t *testing.T) {
	if _, err := exec.LookPath("rustc"); err != nil {
		t.Skip("rustc not installed")
	}
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	outDir := filepath.Join(root, "tests", "machine", "x", "rust")
	os.MkdirAll(outDir, 0755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatal(err)
			}
			prog, err := parser.ParseString(string(data))
			if err != nil {
				writeError(outDir, name, string(data), err)
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				writeError(outDir, name, string(data), errs[0])
				return
			}
			code, err := rustcode.New(env).Compile(prog)
			if err != nil {
				writeError(outDir, name, string(data), err)
				return
			}
			srcFile := filepath.Join(outDir, name+".rs")
			os.WriteFile(srcFile, code, 0644)
			bin := filepath.Join(outDir, name)
			cmd := exec.Command("rustc", srcFile, "-O", "-o", bin)
			var buf bytes.Buffer
			cmd.Stderr = &buf
			if err := cmd.Run(); err != nil {
				writeError(outDir, name, string(code), fmt.Errorf("compile: %v\n%s", err, buf.String()))
				return
			}
			run := exec.Command(bin)
			out, err := run.CombinedOutput()
			if err != nil {
				writeError(outDir, name, string(code), fmt.Errorf("run: %v\n%s", err, out))
				return
			}
			outPath := filepath.Join(outDir, name+".out")
			os.WriteFile(outPath, bytes.TrimSpace(out), 0644)
			os.Remove(bin)
			errFile := filepath.Join(outDir, name+".error")
			os.Remove(errFile)
		})
	}
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}

func updateReadme() {
	root := findRepoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "rust")
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
	buf.WriteString("# Mochi to Rust Machine Outputs\n\n")
	buf.WriteString("This directory contains Rust code generated from the Mochi programs in `tests/vm/valid` using the Rust compiler. Each file was built with `rustc` and executed. Successful runs have a `.out` file, while failures produce a `.error` file.\n\n")
	fmt.Fprintf(&buf, "Compiled programs: %d/%d\n\n", compiled, total)
	buf.WriteString("## Checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n\n## Remaining Tasks\n\n")
	buf.WriteString("- [ ] Implement support for dataset joins that currently fail to compile\n")
	buf.WriteString("- [ ] Handle loading and saving external data\n")
	buf.WriteString("- [ ] Improve error messages for complex pattern matches\n")
	buf.WriteString("- [ ] Add support for generics in function definitions\n")
	buf.WriteString("- [ ] Optimize tail-recursive calls\n")
	buf.WriteString("- [ ] Implement module system for multi-file programs\n")
	buf.WriteString("- [ ] Integrate with Cargo for dependency management\n")
	buf.WriteString("- [ ] Support async/await syntax\n")
	buf.WriteString("- [ ] Provide standard library for common utilities\n")
	buf.WriteString("- [ ] Enhance type inference for nested structs\n")
	buf.WriteString("- [ ] Implement benchmarking harness\n")
	buf.WriteString("- [ ] Add code formatting similar to rustfmt\n")
	_ = os.WriteFile(filepath.Join(outDir, "README.md"), buf.Bytes(), 0o644)
}
