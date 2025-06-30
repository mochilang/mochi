package fuzz

import (
	"io"
	"testing"

	"mochi/parser"
	vm "mochi/runtime/vm"
	"mochi/types"
)

// FuzzVM compiles and executes arbitrary Mochi programs. Invalid
// programs are skipped. Any panic or crash inside the VM will be
// surfaced by the Go fuzzing engine.
func FuzzVM(f *testing.F) {
	gen := NewGenerator()
	for {
		src, ok := gen.Next()
		if !ok {
			break
		}
		f.Add(src)
	}

	f.Fuzz(func(t *testing.T, src string) {
		prog, err := parser.ParseString(src)
		if err != nil {
			t.Skip()
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			t.Skip()
		}
		p, err := vm.Compile(prog, env)
		if err != nil {
			t.Skip()
		}
		m := vm.New(p, io.Discard)
		_ = m.Run()
	})
}
