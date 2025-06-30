package fuzz

import (
	"io"
	"testing"

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func FuzzVM(f *testing.F) {
	seeds := []string{
		"print(1)",
		"let x = 1 + 2\nprint(x)",
	}
	for _, s := range seeds {
		f.Add(s)
	}

	f.Fuzz(func(t *testing.T, src string) {
		defer func() {
			if r := recover(); r != nil {
				t.Fatalf("panic: %v", r)
			}
		}()

		prog, err := parser.ParseString(src)
		if err != nil {
			return
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return
		}
		p, err := vm.Compile(prog, env)
		if err != nil {
			return
		}
		m := vm.New(p, io.Discard)
		_ = m.Run()
	})
}
