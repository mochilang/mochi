//go:build slow

package fs

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	fscode "mochi/archived/x/fs"
	"mochi/parser"
	"mochi/runtime/vm"
	any2mochi "mochi/archived/tools/any2mochi"
	"mochi/types"
)

func compileMochiToFS(path string) (out []byte, err error) {
	defer func() {
		if r := recover(); r != nil {
			err = fmt.Errorf("panic: %v", r)
		}
	}()
	prog, perr := parser.Parse(path)
	if perr != nil {
		return nil, fmt.Errorf("parse error: %w", perr)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	out, err = fscode.New(env).Compile(prog)
	if err != nil {
		return nil, fmt.Errorf("compile error: %w", err)
	}
	return out, nil
}

func TestFSRoundtripVM(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}
	status := make(map[string]string)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			fsSrc, err := compileMochiToFS(src)
			if err != nil {
				status[name] = err.Error()
				return
			}
			dir := t.TempDir()
			fsFile := filepath.Join(dir, name+".fs")
			if err := os.WriteFile(fsFile, fsSrc, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			mochiSrc, err := ConvertFile(fsFile)
			if err != nil {
				status[name] = err.Error()
				return
			}
			prog2, err := parser.ParseString(string(mochiSrc))
			if err != nil {
				status[name] = fmt.Sprintf("parse2 error: %v", err)
				return
			}
			env2 := types.NewEnv(nil)
			if errs := types.Check(prog2, env2); len(errs) > 0 {
				status[name] = fmt.Sprintf("type2 error: %v", errs[0])
				return
			}
			p2, err := vm.CompileWithSource(prog2, env2, string(mochiSrc))
			if err != nil {
				status[name] = fmt.Sprintf("vm compile error: %v", err)
				return
			}
			var buf bytes.Buffer
			m := vm.New(p2, &buf)
			if err := m.Run(); err != nil {
				if ve, ok := err.(*vm.VMError); ok {
					status[name] = fmt.Sprintf("vm run error:\n%s", ve.Format(p2))
				} else {
					status[name] = fmt.Sprintf("vm run error: %v", err)
				}
				return
			}
			status[name] = ""
		})
	}
	any2mochi.WriteStatusMarkdown(filepath.Join(root, "tests", "any2mochi", "fs_vm"), status)
}
