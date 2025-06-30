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

// FuzzQueries generates random dataset queries and compiles them with the VM.
func FuzzQueries(f *testing.F) {
	g := gen.New(rand.New(rand.NewSource(time.Now().UnixNano())))
	for i := 0; i < 50; i++ {
		f.Add(g.Query(3))
	}

	f.Fuzz(func(t *testing.T, q string) {
		prog, err := parser.ParseString("let _ = " + q)
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
