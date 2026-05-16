package mig

import "testing"

func TestFillProbe(t *testing.T) {
	vm := NewVM()
	for _, n := range []int64{1, 8, 128, 1024} {
		if got := vm.FillProbe(n); got != n {
			t.Fatalf("FillProbe(%d) = %d, want %d", n, got, n)
		}
	}
}

func TestMigration(t *testing.T) {
	vm := NewVM()
	xs := vm.newSet(4)
	vm.setAdd(xs, CInt(1))
	if _, ok := vm.Objects[xs.Ptr()].(*vmSetI64); !ok {
		t.Fatal("expected vmSetI64")
	}
	vm.setAdd(xs, CPtr(0))
	if _, ok := vm.Objects[xs.Ptr()].(*vmSet); !ok {
		t.Fatal("expected migration to vmSet")
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
