//go:build slow

package scala

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	scalacode "mochi/compile/x/scala"
	"mochi/parser"
	vm "mochi/runtime/vm"
	any2mochi "mochi/tools/any2mochi"
	"mochi/types"
)

func TestRoundTripVM(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	pattern := filepath.Join(root, "tests/vm/valid", "*.mochi")
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
		if err := roundTripVM(src); err != nil {
			statuses = append(statuses, fmt.Sprintf("%s: %v", name, err))
		} else {
			statuses = append(statuses, fmt.Sprintf("%s: ok", name))
		}
	}
	writeStatusMarkdown(filepath.Join(root, "tests/any2mochi/scala_vm"), statuses)
}

func roundTripVM(path string) error {
	prog, err := parser.Parse(path)
	if err != nil {
		return fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return fmt.Errorf("type error: %v", errs[0])
	}
	code, err := scalacode.New(env).Compile(prog)
	if err != nil {
		return fmt.Errorf("compile error: %w", err)
	}
	mochiSrc, err := Convert(string(code))
	if err != nil {
		return fmt.Errorf("convert error: %w", err)
	}
	prog2, err := parser.ParseString(string(mochiSrc))
	if err != nil {
		return fmt.Errorf("roundtrip parse error: %w", err)
	}
	env2 := types.NewEnv(nil)
	if errs := types.Check(prog2, env2); len(errs) > 0 {
		return fmt.Errorf("roundtrip type error: %v", errs[0])
	}
	p, err := vm.Compile(prog2, env2)
	if err != nil {
		return fmt.Errorf("vm compile error: %w", err)
	}
	var buf bytes.Buffer
	m := vm.New(p, &buf)
	if err := m.Run(); err != nil {
		if ve, ok := err.(*vm.VMError); ok {
			return fmt.Errorf("vm run error:\n%s", ve.Format(p))
		}
		return fmt.Errorf("vm run error: %v", err)
	}
	return nil
}

func writeStatusMarkdown(dir string, lines []string) {
	_ = os.MkdirAll(dir, 0755)
	path := filepath.Join(dir, "ERRORS.md")
	var buf strings.Builder
	buf.WriteString("# Errors\n\n")
	if len(lines) == 0 {
		buf.WriteString("None\n")
	} else {
		for _, l := range lines {
			buf.WriteString("- " + l + "\n")
		}
	}
	_ = os.WriteFile(path, []byte(buf.String()), 0644)
}
