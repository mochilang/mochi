//go:build ts_vm

package ts

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	tscode "mochi/archived/ts"
	"mochi/parser"
	"mochi/runtime/mod"
	"mochi/runtime/vm"
	"mochi/types"
)

func compileMochiToTS(path string) ([]byte, error) {
	prog, err := parser.Parse(path)
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	modRoot, _ := mod.FindRoot(filepath.Dir(path))
	if modRoot == "" {
		modRoot = filepath.Dir(path)
	}
	code, err := tscode.New(env, modRoot).Compile(prog)
	if err != nil {
		return nil, fmt.Errorf("compile error: %w", err)
	}
	return code, nil
}

func runCompileConvertRoundTrip(t *testing.T, dir, pattern string) map[string]string {
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
			defer func() {
				if r := recover(); r != nil {
					errMsg = fmt.Sprintf("panic: %v", r)
				}
			}()
			langCode, err := compileMochiToTS(src)
			if err == nil {
				tmpDir := t.TempDir()
				tmpFile := filepath.Join(tmpDir, name+".ts")
				if wErr := os.WriteFile(tmpFile, langCode, 0644); wErr != nil {
					t.Fatalf("write temp: %v", wErr)
				}
				var out []byte
				out, err = ConvertFile(tmpFile)
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
	var buf strings.Builder
	buf.WriteString("# Errors\n\n")
	if len(status) == 0 {
		buf.WriteString("None\n")
	} else {
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
	}
	_ = os.WriteFile(path, []byte(buf.String()), 0644)
}

func findRepoRoot(t *testing.T) string {
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

func TestTS_VM_RoundTrip(t *testing.T) {
	if err := tscode.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}
	root := findRepoRoot(t)
	status := runCompileConvertRoundTrip(t, filepath.Join(root, "tests/vm/valid"), "*.mochi")
	writeStatusMarkdown(filepath.Join(root, "tests/any2mochi/ts_vm"), status)
}
