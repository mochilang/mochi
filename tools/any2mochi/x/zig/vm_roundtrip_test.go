//go:build zig_vm

package zig

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	zigcode "mochi/archived/x/zig"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func compileMochiToZig(path string) ([]byte, error) {
	prog, err := parser.Parse(path)
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	code, err := zigcode.New(env).Compile(prog)
	if err != nil {
		return nil, fmt.Errorf("compile error: %w", err)
	}
	return code, nil
}

func findRepoRoot(t *testing.T) string {
	t.Helper()
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
	t.Fatal("go.mod not found (not in Go module)")
	return ""
}

func runCompileConvertRunStatus(t *testing.T, dir, pattern string, compile func(string) ([]byte, error), convert func(string) ([]byte, error), lang string) map[string]string {
	files, err := filepath.Glob(filepath.Join(dir, pattern))
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", filepath.Join(dir, pattern))
	}
	status := make(map[string]string)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), filepath.Ext(src))
		var errMsg string
		t.Run(name, func(t *testing.T) {
			langCode, err := compile(src)
			if err == nil {
				tmpDir := t.TempDir()
				tmpFile := filepath.Join(tmpDir, name+"."+lang)
				if wErr := os.WriteFile(tmpFile, langCode, 0644); wErr != nil {
					t.Fatalf("write temp: %v", wErr)
				}
				var out []byte
				out, err = convert(tmpFile)
				if err == nil {
					prog, pErr := parser.ParseString(string(out))
					if pErr != nil {
						err = fmt.Errorf("parse error: %w", pErr)
					} else {
						env := types.NewEnv(nil)
						if errs := types.Check(prog, env); len(errs) > 0 {
							err = fmt.Errorf("type error: %v", errs[0])
						} else if p2, vErr := vm.CompileWithSource(prog, env, string(out)); vErr != nil {
							err = fmt.Errorf("vm compile error: %w", vErr)
						} else {
							var buf bytes.Buffer
							m := vm.New(p2, &buf)
							if rErr := m.Run(); rErr != nil {
								if ve, ok := rErr.(*vm.VMError); ok {
									err = fmt.Errorf("vm run error:\n%s", ve.Format(p2))
								} else {
									err = fmt.Errorf("vm run error: %v", rErr)
								}
							}
						}
					}
				}
			}
			if err != nil {
				errMsg = err.Error()
			}
		})
		status[name] = errMsg
	}
	return status
}

func writeStatusMarkdown(dir string, status map[string]string) {
	_ = os.MkdirAll(dir, 0755)
	path := filepath.Join(dir, "ERRORS.md")
	var buf bytes.Buffer
	buf.WriteString("# Errors\n\n")
	names := make([]string, 0, len(status))
	for n := range status {
		names = append(names, n)
	}
	sort.Strings(names)
	for _, n := range names {
		if msg := status[n]; msg != "" {
			buf.WriteString("- " + n + ": " + msg + "\n")
		} else {
			buf.WriteString("- " + n + ": ok\n")
		}
	}
	_ = os.WriteFile(path, buf.Bytes(), 0644)
}

func TestZig_VM_RoundTrip(t *testing.T) {
	if _, err := zigcode.EnsureZig(); err != nil {
		t.Skipf("zig compiler not installed: %v", err)
	}
	root := findRepoRoot(t)
	status := runCompileConvertRunStatus(
		t,
		filepath.Join(root, "tests/vm/valid"),
		"*.mochi",
		compileMochiToZig,
		ConvertFile,
		"zig",
	)
	writeStatusMarkdown(filepath.Join(root, "tests/any2mochi/zig_vm"), status)
}
