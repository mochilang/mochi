//go:build darwin && arm64

package tracejit

import (
	"testing"

	"mochi/runtime/jit/tmpljit"
)

// reference closed form: sum_{i=0..n-1} (i*2 + 3) = n*(n-1) + 3*n.
func want(n int64) int64 { return n*(n-1) + 3*n }

func TestEngineMatchesClosedForm(t *testing.T) {
	// n must be large enough that the recorder fires (TraceThreshold
	// = 8 iterations) so the JIT path is actually exercised. For
	// completeness we also run a small n that stays under threshold
	// and exits via the pure interpreter path.
	for _, n := range []int64{1, 8, 64, 128, 1024, 4096} {
		e := New(tmpljit.FillSumProgram())
		got, w := e.Run(n), want(n)
		e.Close()
		if got != w {
			t.Fatalf("Engine.Run(%d) = %d, want %d", n, got, w)
		}
	}
}

func TestEngineCompilesOnHotLoop(t *testing.T) {
	// After a single call with n well above TraceThreshold the
	// engine must have a compiled trace cached for the loop header.
	e := New(tmpljit.FillSumProgram())
	defer e.Close()
	if got := e.Run(1024); got != want(1024) {
		t.Fatalf("Run(1024) = %d, want %d", got, want(1024))
	}
	if len(e.traces) != 1 {
		t.Fatalf("expected exactly 1 compiled trace, have %d", len(e.traces))
	}
	for hdr, ct := range e.traces {
		if hdr != ct.trace.HeaderPC {
			t.Fatalf("trace keyed by %d but trace.HeaderPC = %d", hdr, ct.trace.HeaderPC)
		}
		if ct.CodeLen() == 0 {
			t.Fatalf("trace has empty code page")
		}
	}
}

func TestEngineReusesCompiledTrace(t *testing.T) {
	// Second Run on the same Engine must not record again (hit
	// counter already past threshold and trace is cached). We
	// assert this indirectly: the result is still correct and no
	// extra entries appear in e.traces.
	e := New(tmpljit.FillSumProgram())
	defer e.Close()
	for i := 0; i < 4; i++ {
		if got := e.Run(1024); got != want(1024) {
			t.Fatalf("iter %d: Run(1024) = %d, want %d", i, got, want(1024))
		}
	}
	if len(e.traces) != 1 {
		t.Fatalf("expected 1 cached trace after repeated runs, have %d", len(e.traces))
	}
}

// Benchmarks compare four backends on the canonical fillsum
// workload at three input sizes. The MEP-30 (whole-function JIT)
// numbers live in tmpljit's own benchmark suite; the appendix in
// MEP-33 cross-references them.

func benchEngine(b *testing.B, n int64) {
	e := MustCompile(tmpljit.FillSumProgram(), n)
	defer e.Close()
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if e.Run(n) != want(n) {
			b.Fatal("wrong result")
		}
	}
}

func BenchmarkEngine128(b *testing.B)   { benchEngine(b, 128) }
func BenchmarkEngine1024(b *testing.B)  { benchEngine(b, 1024) }
func BenchmarkEngine10000(b *testing.B) { benchEngine(b, 10000) }
