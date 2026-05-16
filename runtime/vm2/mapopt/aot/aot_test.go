package aot

import "testing"

func TestFillSumCorrectness(t *testing.T) {
	vm := NewVM()
	for _, n := range []int64{1, 8, 128, 1024} {
		want := n * (n - 1) / 2
		if got := vm.FillSum(n); got != want {
			t.Fatalf("FillSum(%d) = %d, want %d", n, got, want)
		}
	}
}

func BenchmarkFillSum128(b *testing.B) {
	vm := NewVM()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if got := vm.FillSum(128); got != 128*127/2 {
			b.Fatalf("sum = %d", got)
		}
	}
}
func BenchmarkFillSum1024(b *testing.B) {
	vm := NewVM()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if got := vm.FillSum(1024); got != 1024*1023/2 {
			b.Fatalf("sum = %d", got)
		}
	}
}
