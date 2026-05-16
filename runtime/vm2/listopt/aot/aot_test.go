package aot

import "testing"

func TestFillSumCorrectness(t *testing.T) {
	vm := NewVM()
	if got := vm.FillSum(128); got != 8128 {
		t.Fatalf("FillSum(128) = %d, want 8128", got)
	}
}

func TestNewListGenericExists(t *testing.T) {
	vm := NewVM()
	c := vm.newListGeneric(4)
	if _, ok := vm.Objects[c.Ptr()].(*vmList); !ok {
		t.Fatalf("newListGeneric did not allocate *vmList")
	}
}

func BenchmarkFillSum128(b *testing.B) {
	vm := NewVM()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if got := vm.FillSum(128); got != 8128 {
			b.Fatalf("sum = %d", got)
		}
	}
}

func BenchmarkFillSum1024(b *testing.B) {
	vm := NewVM()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if got := vm.FillSum(1024); got != 523776 {
			b.Fatalf("sum = %d", got)
		}
	}
}
