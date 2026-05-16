package baseline

import "testing"

func TestFillSumGeneric(t *testing.T) {
	vm := NewVM()
	if got := vm.FillSumGeneric(128); got != 8128 {
		t.Fatalf("FillSumGeneric(128) = %d, want 8128", got)
	}
}

func TestFillSumRaw(t *testing.T) {
	if got := FillSumRaw(128); got != 8128 {
		t.Fatalf("FillSumRaw(128) = %d, want 8128", got)
	}
}

func BenchmarkFillSum128(b *testing.B) {
	vm := NewVM()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if got := vm.FillSumGeneric(128); got != 8128 {
			b.Fatalf("sum = %d", got)
		}
	}
}

func BenchmarkFillSum1024(b *testing.B) {
	vm := NewVM()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if got := vm.FillSumGeneric(1024); got != 523776 {
			b.Fatalf("sum = %d", got)
		}
	}
}

func BenchmarkFillSumRaw128(b *testing.B) {
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if got := FillSumRaw(128); got != 8128 {
			b.Fatalf("sum = %d", got)
		}
	}
}

func BenchmarkFillSumRaw1024(b *testing.B) {
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if got := FillSumRaw(1024); got != 523776 {
			b.Fatalf("sum = %d", got)
		}
	}
}
