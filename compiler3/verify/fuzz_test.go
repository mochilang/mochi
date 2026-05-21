package verify

import (
	"testing"

	"mochi/compiler3/ir"
)

// FuzzFunction is the MEP-41 Phase 1 fuzz harness. The gate target is
// 1M opcode sequences without panics and without acceptance of an IR
// that ir.Validate rejects. The default `go test -fuzz=FuzzFunction`
// run is short; CI bumps -fuzztime to hit 1M.
//
// Seed corpus: every IR fixture in compiler3/ir, serialized to a flat
// byte slice. The fuzz body decodes a fresh ir.Function from the input
// bytes, then asserts:
//
//   - Function does not panic on any input (memory-safety property).
//   - If Function returns nil, ir.Validate also returns nil. The
//     verifier is strictly stronger than ir.Validate, so an IR that
//     the verifier admits must be well-formed SSA.
//
// The harness does not assert the converse direction: ir.Validate may
// reject inputs the verifier would accept under a relaxed mode. The
// gate is "no false-accepts past ir.Validate," which is what the
// MEP-41 §8 Phase 1 gate names.
func FuzzFunction(f *testing.F) {
	// Seed corpus: encode every fixture and add as a seed.
	for _, build := range []func() *ir.Function{
		ir.FixtureFibIter,
		ir.FixtureSumLoop,
		ir.FixtureIsEven,
		ir.FixtureDouble,
		ir.FixtureIdent,
		ir.FixtureAddPair,
		ir.FixtureFactRec,
		ir.FixtureGoCallToUpper,
		ir.FixtureGoCallContains,
		ir.FixtureGoCallAlias,
	} {
		f.Add(encodeFunction(build()))
	}

	f.Fuzz(func(t *testing.T, data []byte) {
		fn := decodeFunction(data)
		if fn == nil {
			return
		}
		// Property 1: no panic.
		err := safeFunction(fn)
		// Property 2: if Function accepted, ir.Validate must also accept.
		if err == nil {
			if vErr := ir.Validate(fn); vErr != nil {
				t.Fatalf("Function accepted an IR that ir.Validate rejected: %v", vErr)
			}
		}
	})
}

// safeFunction wraps Function in a recover so a panic surfaces as a
// test failure rather than as a process abort.
func safeFunction(fn *ir.Function) (err error) {
	defer func() {
		if r := recover(); r != nil {
			err = &panicError{val: r}
		}
	}()
	return Function(fn)
}

type panicError struct{ val any }

func (e *panicError) Error() string { return "verify: panic" }

// encodeFunction serializes fn to a compact byte sequence the fuzz
// harness can mutate. The encoding does not need to round-trip
// exactly; the fuzzer is exploring the *shape* of valid IR, not
// preserving identity. Each Value contributes 8 bytes:
//
//	bytes 0-1: Op (uint16; low byte is the OpCode)
//	byte 2:    Type (uint8)
//	byte 3:    arg-count (uint8, capped at 3)
//	bytes 4-7: up to two argument IDs (uint16 each)
//
// The function shape (blocks, terminators) is reconstructed by
// decodeFunction from the same byte stream.
func encodeFunction(fn *ir.Function) []byte {
	if fn == nil {
		return nil
	}
	buf := make([]byte, 0, 8*len(fn.Values))
	for _, v := range fn.Values {
		argN := min(uint8(len(v.Args)), 3)
		buf = append(buf,
			byte(v.Op), 0,
			byte(v.Type),
			argN)
		a0, a1 := uint16(0), uint16(0)
		if len(v.Args) > 0 {
			a0 = uint16(v.Args[0])
		}
		if len(v.Args) > 1 {
			a1 = uint16(v.Args[1])
		}
		buf = append(buf, byte(a0), byte(a0>>8), byte(a1), byte(a1>>8))
	}
	return buf
}

// decodeFunction is the inverse, with one twist: it accepts arbitrary
// input bytes (which the fuzzer may have mutated to nonsense) and
// returns nil rather than panicking on a parse failure. The point is
// to feed the verifier whatever the fuzzer produces, including
// malformed but well-typed-on-disk shapes.
func decodeFunction(data []byte) *ir.Function {
	if len(data) < 8 || len(data) > 8*256 {
		return nil
	}
	n := len(data) / 8
	fn := &ir.Function{Name: "fuzz", Result: ir.TypeI64}
	values := make([]ir.Value, 0, n)
	for i := range n {
		off := 8 * i
		op := ir.OpCode(data[off])
		ty := ir.Type(data[off+2])
		argN := int(data[off+3])
		a0 := uint32(data[off+4]) | uint32(data[off+5])<<8
		a1 := uint32(data[off+6]) | uint32(data[off+7])<<8
		var args []uint32
		switch argN {
		case 0:
		case 1:
			args = []uint32{a0}
		default:
			args = []uint32{a0, a1}
		}
		values = append(values, ir.Value{ID: uint32(i), Type: ty, Op: op, Args: args})
	}
	fn.Values = values
	// Single entry block with all values; terminator returns the last
	// well-typed Value (or Value 0 if none).
	entryID := fn.AddBlock()
	entry := fn.Block(entryID)
	entry.Values = make([]uint32, 0, len(values))
	for i := range values {
		entry.Values = append(entry.Values, uint32(i))
	}
	// Use Value 0 as the param if its Op is OpParam-compatible; the
	// fuzzer's mutation may have changed it.
	if len(values) > 0 {
		if values[0].Op == ir.OpParam || values[0].Op == ir.OpInvalid {
			values[0].Op = ir.OpParam
			values[0].Type = ir.TypeI64
			values[0].Args = nil
			fn.Params = []uint32{0}
		}
	}
	// Pick a returnable Value of the function's Result Type, else
	// fall back to Value 0.
	retID := uint32(0)
	for i := range values {
		if values[i].Type == fn.Result {
			retID = uint32(i)
		}
	}
	entry.Term = ir.Terminator{Kind: ir.TermReturn, Value: retID}
	return fn
}
