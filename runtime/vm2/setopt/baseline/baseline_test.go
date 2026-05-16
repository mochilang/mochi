package baseline

import "testing"

func TestFillProbe(t *testing.T) {
	vm := NewVM()
	for _, n := range []int64{1, 8, 128, 1024} {
		if got := vm.FillProbeGeneric(n); got != n {
			t.Fatalf("Generic(%d) = %d, want %d", n, got, n)
		}
		if got := FillProbeRaw(n); got != n {
			t.Fatalf("Raw(%d) = %d, want %d", n, got, n)
		}
	}
}

func BenchmarkRaw128(b *testing.B) {
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if FillProbeRaw(128) != 128 {
			b.Fatal()
		}
	}
}
func BenchmarkRaw1024(b *testing.B) {
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if FillProbeRaw(1024) != 1024 {
			b.Fatal()
		}
	}
}
func BenchmarkGeneric128(b *testing.B) {
	vm := NewVM()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if vm.FillProbeGeneric(128) != 128 {
			b.Fatal()
		}
	}
}
func BenchmarkGeneric1024(b *testing.B) {
	vm := NewVM()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if vm.FillProbeGeneric(1024) != 1024 {
			b.Fatal()
		}
	}
}
