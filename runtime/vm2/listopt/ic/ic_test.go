package ic

import "testing"

func TestFillSumCorrectness(t *testing.T) {
	vm := NewVM()
	if got := vm.FillSum(128); got != 8128 {
		t.Fatalf("FillSum(128) = %d, want 8128", got)
	}
}

// TestICStaysMonomorphic asserts that on a single-shape workload the
// push IC ends in state 1 (cached *vmListI64) — the property the
// IC's hot path optimization depends on.
func TestICStaysMonomorphic(t *testing.T) {
	vm := NewVM()
	var pushSlot ICSlot
	xs := vm.newList(4)
	for i := int64(0); i < 8; i++ {
		vm.listPushIC(&pushSlot, xs, CInt(i))
	}
	if pushSlot.State != 1 {
		t.Fatalf("push IC state = %d, want 1 (monomorphic *vmListI64)", pushSlot.State)
	}
}

func TestICMigratesOnNonInt(t *testing.T) {
	vm := NewVM()
	var pushSlot ICSlot
	xs := vm.newList(4)
	vm.listPushIC(&pushSlot, xs, CInt(10))
	vm.listPushIC(&pushSlot, xs, CPtr(99))
	if pushSlot.State != 2 {
		t.Fatalf("push IC state = %d after migrate, want 2", pushSlot.State)
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
