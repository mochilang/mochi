//go:build slow

package vm_test

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"testing"

	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestVMValidPrograms(t *testing.T) {
	golden.Run(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		wd, _ := os.Getwd()
		os.Chdir(filepath.Join(filepath.Dir(src), ".."))
		defer os.Chdir(wd)

		prog, err := parser.Parse(src)
		if err != nil {
			writeErr(src, err)
			return nil, fmt.Errorf("parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			writeErr(src, errs[0])
			return nil, fmt.Errorf("type error: %v", errs[0])
		}
		p, err := vm.Compile(prog, env)
		if err != nil {
			writeErr(src, err)
			return nil, fmt.Errorf("compile error: %w", err)
		}
		var out bytes.Buffer
		m := vm.New(p, &out)
		if err := m.Run(); err != nil {
			writeErr(src, err)
			return nil, fmt.Errorf("run error: %w", err)
		}
		removeErr(src)
		return bytes.TrimSpace(out.Bytes()), nil
	})
}

func TestVMIRGolden(t *testing.T) {
	golden.Run(t, "tests/vm/valid", ".mochi", ".ir.out", func(src string) ([]byte, error) {
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
		data, err := os.ReadFile(src)
		if err != nil {
			return nil, err
		}
		ir := p.Disassemble(string(data))
		return []byte(ir), nil
	})
}
