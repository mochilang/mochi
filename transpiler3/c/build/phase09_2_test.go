package build

import (
	"runtime"
	"testing"
)

// TestPhase9Stream is the MEP-45 Phase 9.2 gate for the MPMC broadcast stream.
// It walks every fixture under tests/transpiler3/c/fixtures/stream and runs the
// same end-to-end pipeline as all other phase gates.
//
// Phase 9.2 adds:
//   - stream<T> and sub<T> as first-class types in the type system (types/kinds.go)
//   - make_stream(cap), emit(s, val), subscribe(s), recv_sub(sub) builtins (types/check.go)
//   - StreamMakeExpr / StreamEmitStmt / SubMakeExpr / SubRecvExpr IR nodes (aotir/program.go)
//   - Full lowering and emission for each typed stream operation (lower, emit)
//   - libmochi runtime: include/mochi/stream.h + src/stream.c implementing an
//     MPMC broadcast ring buffer with per-subscriber cursors and cooperative yield.
//
// The fixtures exercise sequential emit-then-recv_sub usage (no fiber blocking
// needed) across all scalar element types and multiple-subscriber broadcast.
//
// Phase 9.2 fixtures:
//   - stream_basic: stream<int> cap 4, emit 3 ints, one subscriber reads all 3.
//   - stream_float: stream<float> cap 4, emit 2 floats, one subscriber reads both.
//   - stream_bool: stream<bool> cap 4, emit true/false/true, one subscriber reads all.
//   - stream_string: stream<string> cap 4, emit "hello"/"world", one subscriber reads both.
//   - stream_multi_sub: stream<int> cap 4, emit 2 ints, two independent subscribers each read both.
func TestPhase9Stream(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 9 stream gate skipped on Windows; ucontext ships POSIX only")
	}
	runFixtureSuite(t, "stream")
}
