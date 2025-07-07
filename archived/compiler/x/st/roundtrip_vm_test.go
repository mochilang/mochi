//go:build archived && stroundtrip

package stcode_test

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	stcode "mochi/archived/x/st"
	"mochi/parser"
	"mochi/runtime/vm"
	stconv "mochi/archived/tools/any2mochi/x/st"
	"mochi/types"
)

func compileMochiToST(path string) ([]byte, error) {
	prog, err := parser.Parse(path)
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	// create dummy gst-format to avoid package installation
	tmpDir, _ := os.MkdirTemp("", "stfmt")
	script := filepath.Join(tmpDir, "gst-format")
	os.WriteFile(script, []byte("#!/bin/sh\ncat"), 0755)
	origPath := os.Getenv("PATH")
	os.Setenv("PATH", tmpDir+":"+origPath)
	defer os.Setenv("PATH", origPath)
	defer func() {
		if r := recover(); r != nil {
			err = fmt.Errorf("panic: %v", r)
		}
	}()
	code, err := stcode.New(env).Compile(prog)
	if err != nil {
		return nil, fmt.Errorf("compile error: %w", err)
	}
	return code, nil
}

func runRoundtripStatus(t *testing.T, dir, pattern string, compile func(string) ([]byte, error), convert func(string) ([]byte, error), lang string) map[string]string {
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

func TestST_VM_RoundTrip(t *testing.T) {
	root := findRepoRoot(t)
	status := runRoundtripStatus(
		t,
		filepath.Join(root, "tests/vm/valid"),
		"*.mochi",
		compileMochiToST,
		stconv.ConvertFile,
		"st",
	)
	writeStatusMarkdown(filepath.Join(root, "compile/x/st"), status)
}
