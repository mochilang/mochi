package fuzz

import (
	"io"
	"math/rand"
	"testing"
	"time"

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/tools/fuzz/gen"
	"mochi/types"
)

// FuzzVMCompile parses input and attempts to compile it with the VM compiler.
// Any errors are ignored; the goal is to catch panics during compilation.
func FuzzVMCompile(f *testing.F) {
	g := gen.New(rand.New(rand.NewSource(time.Now().UnixNano())))
	for i := 0; i < 50; i++ {
		f.Add(g.Program(100))
	}

	f.Fuzz(func(t *testing.T, src string) {
		prog, err := parser.ParseString(src)
		if err != nil {
			return
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return
		}
		p, errc := vm.Compile(prog, env)
		if errc != nil {
			return
		}
		m := vm.New(p, io.Discard)
		_ = m.Run()
	})
}
