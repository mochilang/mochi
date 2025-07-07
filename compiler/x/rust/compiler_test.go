package rscode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
	"testing"

	rscode "mochi/compiler/x/rust"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

// compileFile compiles a Mochi source file to Rust code.
func compileFile(path string) ([]byte, error) {
	prog, err := parser.Parse(path)
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	code, err := rscode.New(env).Compile(prog)
	if err != nil {
		return nil, fmt.Errorf("compile error: %w", err)
	}
	return code, nil
}

// findRoot walks up to find go.mod
func findRoot(t *testing.T) string {
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

func TestRustCompiler_VMValid(t *testing.T) {
	if err := rscode.Ensure(); err != nil {
		t.Skipf("rust not installed: %v", err)
	}
	root := findRoot(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "rust")
	if err := os.MkdirAll(outDir, 0755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}

	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	sort.Strings(files)
	results := make(map[string]bool)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			code, err := compileFile(src)
			codePath := filepath.Join(outDir, name+".rs")
			if err == nil {
				if err := os.WriteFile(codePath, code, 0644); err != nil {
					t.Fatalf("write code: %v", err)
				}
				exe := filepath.Join(t.TempDir(), name)
				ccmd := exec.Command("rustc", codePath, "-O", "-o", exe)
				cOut, cErr := ccmd.CombinedOutput()
				if cErr == nil {
					rcmd := exec.Command(exe)
					out, runErr := rcmd.CombinedOutput()
					if runErr == nil {
						results[name] = true
						outPath := filepath.Join(outDir, name+".out")
						os.WriteFile(outPath, bytes.TrimSpace(out), 0644)
						// compare with VM output
						vmOut, err := runVM(src)
						if err == nil {
							if !bytes.Equal(bytes.TrimSpace(out), vmOut) {
								t.Errorf("output mismatch for %s", name)
							}
						}
						return
					}
					err = fmt.Errorf("run error: %w\n%s", runErr, out)
				} else {
					err = fmt.Errorf("rustc error: %w\n%s", cErr, cOut)
				}
			}
			// failure path
			results[name] = false
			errPath := filepath.Join(outDir, name+".error")
			writeError(errPath, err, codePath)
		})
	}
	updateREADME(outDir, files, results)
}

// runVM executes the Mochi VM for the given source file and returns trimmed stdout.
func runVM(src string) ([]byte, error) {
	prog, err := parser.Parse(src)
	if err != nil {
		return nil, err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, errs[0]
	}
	p, err := vm.Compile(prog, env)
	if err != nil {
		return nil, err
	}
	var buf bytes.Buffer
	m := vm.New(p, &buf)
	if err := m.Run(); err != nil {
		return nil, err
	}
	return bytes.TrimSpace(buf.Bytes()), nil
}

// writeError writes an error file with context from the source code.
func writeError(path string, err error, src string) {
	msg := err.Error()
	var line int
	if m := regexp.MustCompile(`:([0-9]+):`).FindStringSubmatch(msg); len(m) == 2 {
		fmt.Sscanf(m[1], "%d", &line)
	}
	var context string
	if data, err := os.ReadFile(src); err == nil {
		lines := strings.Split(string(data), "\n")
		start := line - 2
		if start < 0 {
			start = 0
		}
		end := line + 1
		if end > len(lines) {
			end = len(lines)
		}
		for i := start; i < end; i++ {
			context += fmt.Sprintf("%d: %s\n", i+1, lines[i])
		}
	}
	out := fmt.Sprintf("line %d\n%s\n%s", line, msg, context)
	os.WriteFile(path, []byte(out), 0644)
}

func updateREADME(dir string, files []string, results map[string]bool) {
	sort.Strings(files)
	var b strings.Builder
	b.WriteString("# Rust Compilation Checklist\n\n")
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		if results[name] {
			b.WriteString(fmt.Sprintf("- [x] %s\n", name))
		} else {
			b.WriteString(fmt.Sprintf("- [ ] %s\n", name))
		}
	}
	os.WriteFile(filepath.Join(dir, "README.md"), []byte(b.String()), 0644)
}
