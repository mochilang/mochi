//go:build slow

package goroundtrip

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"testing"

	gocode "mochi/archived/go"
	"mochi/parser"
	"mochi/runtime/vm"
	any2mochi "mochi/archived/tools/any2mochi"
	golang "mochi/archived/tools/any2mochi/go"
	"mochi/types"
)

func TestRoundTripVMValid(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}
	var statuses []string
	for _, src := range files {
		name := filepath.Base(src)
		if err := processFile(t, src); err != nil {
			statuses = append(statuses, fmt.Sprintf("%s: %v", name, err))
		} else {
			statuses = append(statuses, fmt.Sprintf("%s: ok", name))
		}
	}
	any2mochi.WriteErrorsMarkdown(filepath.Join(root, "tests", "any2mochi", "go"), statuses)
}

func processFile(t *testing.T, src string) error {
	prog, err := parser.Parse(src)
	if err != nil {
		return fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return fmt.Errorf("type error: %v", errs[0])
	}
	code, err := gocode.New(env).Compile(prog)
	if err != nil {
		return fmt.Errorf("go compile error: %w", err)
	}
	tmp := t.TempDir()
	goFile := filepath.Join(tmp, "main.go")
	if err := os.WriteFile(goFile, code, 0644); err != nil {
		return err
	}
	mochiSrc, err := golang.ConvertFile(goFile)
	if err != nil {
		return fmt.Errorf("convert error: %w", err)
	}
	prog2, err := parser.ParseString(string(mochiSrc))
	if err != nil {
		return fmt.Errorf("parse2 error: %w", err)
	}
	env2 := types.NewEnv(nil)
	if errs := types.Check(prog2, env2); len(errs) > 0 {
		return fmt.Errorf("type2 error: %v", errs[0])
	}
	p, err := vm.Compile(prog2, env2)
	if err != nil {
		return fmt.Errorf("vm compile error: %w", err)
	}
	var out bytes.Buffer
	m := vm.New(p, &out)
	if err := m.Run(); err != nil {
		return fmt.Errorf("vm run error: %w", err)
	}
	_ = out
	return nil
}
