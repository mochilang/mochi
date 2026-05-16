package ic

import "testing"

func TestFillProbe(t *testing.T) {
	vm := NewVM()
	for _, n := range []int64{1, 8, 128, 1024} {
		if got := vm.FillProbe(n); got != n {
			t.Fatalf("FillProbe(%d) = %d, want %d", n, got, n)
		}
	}
}

func TestICStaysMonomorphic(t *testing.T) {
	vm := NewVM()
	vm.Objects = vm.Objects[:0]
	var addSlot, hasSlot ICSlot
	xs := vm.newSet(128)
	for i := int64(0); i < 128; i++ {
		vm.setAddIC(&addSlot, xs, CInt(i))
	}
	for i := int64(0); i < 128; i++ {
		vm.setHasIC(&hasSlot, xs, CInt(i))
	}
	if addSlot.State != 1 || hasSlot.State != 1 {
		t.Fatalf("expected state=1, got add=%d has=%d", addSlot.State, hasSlot.State)
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
