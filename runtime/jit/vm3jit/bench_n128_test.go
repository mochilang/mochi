package vm3jit_test

import (
	"testing"

	"mochi/compiler3/corpus"
	"mochi/runtime/jit/vm3jit"
	"mochi/runtime/vm3"
)

func BenchmarkListsFillSumN128NoParity(b *testing.B) {
	prog := corpus.ListsFillSum.Build(128)
	cfs := vm3jit.CompileProgram(prog)
	defer func() {
		for _, cf := range cfs {
			if cf != nil {
				_ = cf.Free()
			}
		}
	}()
	fn := prog.Funcs[prog.Entry]
	if fn.JITCode == nil {
		b.Skip("main not JIT-compiled on this host")
	}
	vm := vm3.NewWithProgram(prog)
	args := []int64{128}
	b.ResetTimer()
	var s int64
	for i := 0; i < b.N; i++ {
		got, err := vm.RunWithArgs(fn, args)
		if err != nil {
			b.Fatalf("%v", err)
		}
		s += got.Int()
	}
	vm3jitN128Sink = s
}

var vm3jitN128Sink int64
