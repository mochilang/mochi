package ic

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

func TestICStaysMonomorphic(t *testing.T) {
	vm := NewVM()
	vm.Objects = vm.Objects[:0]
	var setSlot, getSlot ICSlot
	xs := vm.newMap(128)
	for i := int64(0); i < 128; i++ {
		vm.mapSetIC(&setSlot, xs, CInt(i), CInt(i))
	}
	for i := int64(0); i < 128; i++ {
		vm.mapGetIC(&getSlot, xs, CInt(i))
	}
	if setSlot.State != 1 || getSlot.State != 1 {
		t.Fatalf("expected monomorphic state=1, got set=%d get=%d", setSlot.State, getSlot.State)
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
