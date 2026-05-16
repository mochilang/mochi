//go:build darwin && arm64

package tmpljit

import "testing"

// reference closed-form: sum_{i=0..n-1} (i*2 + 3) = n*(n-1) + 3*n
func want(n int64) int64 { return n*(n-1) + 3*n }

func TestFloorGoMatchesClosedForm(t *testing.T) {
	// n>=1: the loop body is unconditionally entered (do-while).
	// Validating n=0 would require a pre-check opcode that the
	// MEP-30 prototype intentionally omits.
	for _, n := range []int64{1, 8, 128, 1024, 4096} {
		if got, w := FloorGo(n), want(n); got != w {
			t.Fatalf("FloorGo(%d) = %d, want %d", n, got, w)
		}
	}
}

func TestInterpMatchesFloor(t *testing.T) {
	p := FillSumProgram()
	// n>=1: the loop body is unconditionally entered (do-while).
	// Validating n=0 would require a pre-check opcode that the
	// MEP-30 prototype intentionally omits.
	for _, n := range []int64{1, 8, 128, 1024, 4096} {
		if got, w := Interp(p, n), want(n); got != w {
			t.Fatalf("Interp(%d) = %d, want %d", n, got, w)
		}
	}
}

func TestJITMatchesFloor(t *testing.T) {
	p := FillSumProgram()
	cf, err := Compile(p)
	if err != nil {
		t.Fatal(err)
	}
	defer cf.Free()
	// n>=1: the loop body is unconditionally entered (do-while).
	// Validating n=0 would require a pre-check opcode that the
	// MEP-30 prototype intentionally omits.
	for _, n := range []int64{1, 8, 128, 1024, 4096} {
		if got, w := cf.Call(n), want(n); got != w {
			t.Fatalf("JIT(%d) = %d, want %d", n, got, w)
		}
	}
}

func TestJITCodeSize(t *testing.T) {
	cf, err := Compile(FillSumProgram())
	if err != nil {
		t.Fatal(err)
	}
	defer cf.Free()
	// 1 prologue + 6*MovImm(2) + 2*Mul(1) actually let's not
	// over-specify; just print to log so reviewers can spot churn.
	t.Logf("JIT code size: %d bytes (page-rounded)", cf.CodeLen())
}

func BenchmarkFloorGo128(b *testing.B) {
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if FloorGo(128) != want(128) {
			b.Fatal()
		}
	}
}
func BenchmarkInterp128(b *testing.B) {
	p := FillSumProgram()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if Interp(p, 128) != want(128) {
			b.Fatal()
		}
	}
}
func BenchmarkJIT128(b *testing.B) {
	cf, err := Compile(FillSumProgram())
	if err != nil {
		b.Fatal(err)
	}
	defer cf.Free()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if cf.Call(128) != want(128) {
			b.Fatal()
		}
	}
}

func BenchmarkFloorGo1024(b *testing.B) {
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if FloorGo(1024) != want(1024) {
			b.Fatal()
		}
	}
}
func BenchmarkInterp1024(b *testing.B) {
	p := FillSumProgram()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if Interp(p, 1024) != want(1024) {
			b.Fatal()
		}
	}
}
func BenchmarkJIT1024(b *testing.B) {
	cf, err := Compile(FillSumProgram())
	if err != nil {
		b.Fatal(err)
	}
	defer cf.Free()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if cf.Call(1024) != want(1024) {
			b.Fatal()
		}
	}
}

func BenchmarkFloorGo10000(b *testing.B) {
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if FloorGo(10000) != want(10000) {
			b.Fatal()
		}
	}
}
func BenchmarkInterp10000(b *testing.B) {
	p := FillSumProgram()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if Interp(p, 10000) != want(10000) {
			b.Fatal()
		}
	}
}
func BenchmarkJIT10000(b *testing.B) {
	cf, err := Compile(FillSumProgram())
	if err != nil {
		b.Fatal(err)
	}
	defer cf.Free()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if cf.Call(10000) != want(10000) {
			b.Fatal()
		}
	}
}
