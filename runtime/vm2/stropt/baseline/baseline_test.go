package baseline

import "testing"

func TestConcatLoopCorrectness(t *testing.T) {
	vm := NewVM()
	if got := vm.ConcatLoop(30); got != 30 {
		t.Fatalf("ConcatLoop(30) = %d, want 30", got)
	}
	if got := vm.ConcatLoop(200); got != 200 {
		t.Fatalf("ConcatLoop(200) = %d, want 200", got)
	}
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
