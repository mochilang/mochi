package baseline

import "testing"

func TestFillSumCorrectness(t *testing.T) {
	vm := NewVM()
	for _, n := range []int64{1, 8, 128, 1024} {
		want := n * (n - 1) / 2
		if got := vm.FillSumGeneric(n); got != want {
			t.Fatalf("Generic(%d) = %d, want %d", n, got, want)
		}
		if got := FillSumRaw(n); got != want {
			t.Fatalf("Raw(%d) = %d, want %d", n, got, want)
		}
	}
}

func BenchmarkFillSumRaw128(b *testing.B) {
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if got := FillSumRaw(128); got != 128*127/2 {
			b.Fatalf("sum = %d", got)
		}
	}
}
func BenchmarkFillSumRaw1024(b *testing.B) {
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if got := FillSumRaw(1024); got != 1024*1023/2 {
			b.Fatalf("sum = %d", got)
		}
	}
}
func BenchmarkFillSumGeneric128(b *testing.B) {
	vm := NewVM()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if got := vm.FillSumGeneric(128); got != 128*127/2 {
			b.Fatalf("sum = %d", got)
		}
	}
}
func BenchmarkFillSumGeneric1024(b *testing.B) {
	vm := NewVM()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if got := vm.FillSumGeneric(1024); got != 1024*1023/2 {
			b.Fatalf("sum = %d", got)
		}
	}
}
