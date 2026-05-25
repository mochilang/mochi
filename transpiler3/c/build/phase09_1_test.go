package build

import (
	"runtime"
	"testing"
)

// TestPhase9Chan is the MEP-45 Phase 9.1 gate for the bounded ring channel.
// It walks every fixture under tests/transpiler3/c/fixtures/chan and runs the
// same end-to-end pipeline as all other phase gates.
//
// Phase 9.1 adds:
//   - chan<T> as a first-class type in the type system (types/kinds.go)
//   - make_chan(cap), send(ch, val), recv(ch) builtins (types/check.go)
//   - ChanMakeExpr / ChanSendStmt / ChanRecvExpr IR nodes (aotir/program.go)
//   - Full lowering and emission for each typed channel operation (lower, emit)
//   - libmochi runtime: include/mochi/chan.h + src/chan.c implementing a
//     bounded ring buffer with cooperative yield when full/empty.
//
// The fixtures exercise sequential send-then-recv usage of buffered channels
// (no inter-fiber communication yet) across all scalar element types.
//
// Phase 9.1 fixtures:
//   - chan_basic: chan<int> cap 1, send one value, recv it.
//   - chan_buffered: chan<int> cap 3, fill buffer then drain in FIFO order.
//   - chan_string: chan<string> cap 2, send two strings, recv both.
//   - chan_bool: chan<bool> cap 2, send true/false, recv both.
//   - chan_fifo_order: chan<int> cap 5, send 1..5, recv and verify FIFO ordering.
func TestPhase9Chan(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 9 chan gate skipped on Windows; ucontext ships POSIX only")
	}
	runFixtureSuite(t, "chan")
}
