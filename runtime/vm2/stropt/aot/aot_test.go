package aot

import "testing"

func TestConcatLoopCorrectness(t *testing.T) {
	vm := NewVM()
	for _, n := range []int64{1, 5, 6, 30, 64, 65, 200, 1000} {
		got := vm.ConcatLoop(n)
		if got != n {
			t.Fatalf("ConcatLoop(%d) = %d, want %d", n, got, n)
		}
		s := vm.flatten(vm.lastResult(n))
		if int64(len(s)) != n {
			t.Fatalf("flatten len = %d, want %d", len(s), n)
		}
		for _, b := range s {
			if b != 'x' {
				t.Fatalf("byte = %q, want 'x'", b)
			}
		}
	}
}

func (vm *VM) lastResult(n int64) Cell {
	vm.Objects = vm.Objects[:0]
	x := CSStr([]byte("x"))
	s := CSStr(nil)
	for i := int64(0); i < n; i++ {
		s = vm.concatAny(s, x)
	}
	return s
}

func BenchmarkConcatLoop30(b *testing.B) {
	vm := NewVM()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if got := vm.ConcatLoop(30); got != 30 {
			b.Fatalf("len = %d", got)
		}
	}
}

func BenchmarkConcatLoop200(b *testing.B) {
	vm := NewVM()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if got := vm.ConcatLoop(200); got != 200 {
			b.Fatalf("len = %d", got)
		}
	}
}

func BenchmarkConcatLoop1000(b *testing.B) {
	vm := NewVM()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if got := vm.ConcatLoop(1000); got != 1000 {
			b.Fatalf("len = %d", got)
		}
	}
}
