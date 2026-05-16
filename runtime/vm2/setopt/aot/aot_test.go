package aot

import "testing"

func TestFillProbe(t *testing.T) {
	vm := NewVM()
	for _, n := range []int64{1, 8, 128, 1024} {
		if got := vm.FillProbe(n); got != n {
			t.Fatalf("FillProbe(%d) = %d, want %d", n, got, n)
		}
	}
}

func BenchmarkFillProbe128(b *testing.B) {
	vm := NewVM()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if vm.FillProbe(128) != 128 {
			b.Fatal()
		}
	}
}
func BenchmarkFillProbe1024(b *testing.B) {
	vm := NewVM()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if vm.FillProbe(1024) != 1024 {
			b.Fatal()
		}
	}
}
