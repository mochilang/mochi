//go:build slow

package cobol_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	cobol "mochi/compiler/x/cobol"
	"mochi/parser"
	"mochi/types"
)

func ensureCobc(t *testing.T) string {
	if p, err := exec.LookPath("cobc"); err == nil {
		return p
	}
	t.Skip("COBOL compiler not found")
	return ""
}

func repoRoot() string {
	_, file, _, _ := runtime.Caller(0)
	return filepath.Clean(filepath.Join(filepath.Dir(file), "..", "..", ".."))
}

func TestCobolCompiler_Programs(t *testing.T) {
	cobc := ensureCobc(t)
	root := repoRoot()
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	outDir := filepath.Join(root, "tests", "machine", "x", "cobol")
	os.MkdirAll(outDir, 0755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			data, _ := os.ReadFile(src)
			errPath := filepath.Join(outDir, name+".error")
			os.Remove(errPath)
			prog, err := parser.Parse(src)
			if err != nil {
				writeError(outDir, name, string(data), err)
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				writeError(outDir, name, string(data), errs[0])
				return
			}
			code, err := cobol.New(env).Compile(prog)
			if err != nil {
				writeError(outDir, name, string(data), err)
				return
			}
			srcFile := filepath.Join(outDir, name+".cob")
			os.WriteFile(srcFile, code, 0644)
			bin := filepath.Join(outDir, name)
			if out, err := exec.Command(cobc, "-x", "-o", bin, srcFile).CombinedOutput(); err != nil {
				writeError(outDir, name, string(code), fmt.Errorf("cobc: %v\n%s", err, out))
				return
			}
			out, err := exec.Command(bin).CombinedOutput()
			if err != nil {
				writeError(outDir, name, string(code), fmt.Errorf("run: %v\n%s", err, out))
				return
			}
			os.WriteFile(filepath.Join(outDir, name+".out"), bytes.TrimSpace(out), 0644)
		})
	}
}

func writeError(dir, name, src string, err error) {
	lines := strings.Split(src, "\n")
	msg := err.Error()
	ln := 0
	if idx := strings.Index(msg, "line "); idx != -1 {
		fmt.Sscanf(msg[idx:], "line %d", &ln)
	}
	var ctx string
	if ln > 0 {
		start := ln - 2
		if start < 0 {
			start = 0
		}
		end := ln + 1
		if end > len(lines) {
			end = len(lines)
		}
		ctx = strings.Join(lines[start:end], "\n")
	}
	os.WriteFile(filepath.Join(dir, name+".error"), []byte(msg+"\n"+ctx), 0644)
}
