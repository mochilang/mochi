package mig

import "testing"

func TestFillSumCorrectness(t *testing.T) {
	vm := NewVM()
	// 0+1+...+127 = 127*128/2 = 8128
	if got := vm.FillSum(128); got != 8128 {
		t.Fatalf("FillSum(128) = %d, want 8128", got)
	}
}

// TestMigrationOnNonInt verifies that pushing a non-int cell migrates
// the list and that subsequent reads return both the int prefix and
// the heterogeneous tail. The migration is the load-bearing behavior
// distinguishing MEP-26 from MEP-28.
func TestMigrationOnNonInt(t *testing.T) {
	vm := NewVM()
	xs := vm.newList(4)
	vm.listPush(xs, CInt(10))
	vm.listPush(xs, CInt(20))
	// Force migration with a non-int Cell (use a synthetic ptr cell).
	vm.listPush(xs, CPtr(99))
	if vm.listLen(xs) != 3 {
		t.Fatalf("len after migrate = %d, want 3", vm.listLen(xs))
	}
	if got := vm.listGet(xs, 0).Int(); got != 10 {
		t.Fatalf("[0] after migrate = %d, want 10", got)
	}
	if got := vm.listGet(xs, 1).Int(); got != 20 {
		t.Fatalf("[1] after migrate = %d, want 20", got)
	}
	if got := vm.listGet(xs, 2).Ptr(); got != 99 {
		t.Fatalf("[2] after migrate = %d, want 99", got)
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
