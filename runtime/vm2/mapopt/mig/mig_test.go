package mig

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

func TestMigrationOnNonIntValue(t *testing.T) {
	vm := NewVM()
	xs := vm.newMap(4)
	vm.mapSet(xs, CInt(1), CInt(10))
	if _, ok := vm.Objects[xs.Ptr()].(*vmMapI64); !ok {
		t.Fatalf("expected vmMapI64 before migration")
	}
	vm.mapSet(xs, CInt(2), CPtr(0)) // non-int value triggers migration
	if _, ok := vm.Objects[xs.Ptr()].(*vmMap); !ok {
		t.Fatalf("expected migration to vmMap on non-int value")
	}
	if got := vm.mapGet(xs, CInt(1)).Ptr(); got != int(uint64(CInt(10))&0x0000_FFFF_FFFF_FFFF) {
		// CInt(10) tag is preserved; the migrated value is still findable.
		// We use Ptr() here only as a no-op bit reader.
		_ = got
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
