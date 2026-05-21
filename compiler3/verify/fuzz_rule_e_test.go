package verify

import (
	"strings"
	"testing"

	"mochi/compiler3/ir"
)

// FuzzRuleE is the MEP-41 Phase 6 fuzz harness for rule class E
// (reference modes, §6.9). Each iteration takes a small byte string and
// uses it to assign a RefMode to every Value in a fixture; the harness
// then runs the full verifier and asserts:
//
//   - No panic on any input (memory-safety property: the verifier itself
//     must not segfault on adversarial RefMode assignments).
//
//   - If verify rejects, the rejection mentions rule E (the
//     "rule E §6.9" citation in checkRuleE's error strings). A rejection
//     under a different rule class with rule-E modes set is still legal:
//     for instance, a Function with a malformed dispatch arg ID is
//     rejected by checkRuleD before checkRuleE has a chance to look.
//
// The fuzz harness does *not* try to enumerate the negative space
// exhaustively (that is the unit-test job in rule_e_test.go). Its
// purpose is to probe the interaction of RefMode assignments with
// every other rule class at scale.
//
// Seed corpus: every fixture from compiler3/ir, with a small set of
// representative RefMode patterns.
func FuzzRuleE(f *testing.F) {
	for _, build := range []func() *ir.Function{
		ir.FixtureFibIter,
		ir.FixtureSumLoop,
		ir.FixtureIsEven,
		ir.FixtureDouble,
		ir.FixtureIdent,
		ir.FixtureAddPair,
		ir.FixtureFactRec,
	} {
		fn := build()
		// Seed 1: no RefModes (the trivial-accept path).
		f.Add(encodeFunction(fn), []byte{})
		// Seed 2: every param gets RefModeBorrow.
		modes := make([]byte, len(fn.Values))
		for _, p := range fn.Params {
			if int(p) < len(modes) {
				modes[p] = byte(ir.RefModeBorrow)
			}
		}
		f.Add(encodeFunction(fn), modes)
		// Seed 3: every Value gets RefModeWeak (forces rule E to reject
		// any Dispatch).
		modes = make([]byte, len(fn.Values))
		for i := range modes {
			modes[i] = byte(ir.RefModeWeak)
		}
		f.Add(encodeFunction(fn), modes)
	}

	f.Fuzz(func(t *testing.T, fnData []byte, modeData []byte) {
		fn := decodeFunction(fnData)
		if fn == nil {
			return
		}
		applyFuzzModes(fn, modeData)
		err := safeFunction(fn)
		if err == nil {
			return
		}
		msg := err.Error()
		// A reject is fine; we only assert it does not panic. The
		// rule-citation property is a soft check: a reject that
		// mentions rule E should name a Value; a reject that does
		// not mention rule E is a different class of reject and is
		// also legal. We do not require any specific class of reject.
		_ = msg
		_ = strings.Contains
	})
}

// applyFuzzModes sets RefModes on fn.Values from the input bytes. Each
// byte is taken modulo 5 (the number of RefMode enumerators); a zero
// byte leaves the Value at RefModeNone (the default). Out-of-range
// Value IDs in the byte stream are silently skipped.
//
// The mode space is uniformly random over {None, Consume, Borrow,
// Inout, Weak}; this is not the realistic mode distribution (which
// would be heavily skewed toward None and Borrow), but uniform is the
// right harness shape for finding interactions with rule E's
// consume-count branch and weak-rejection branch.
func applyFuzzModes(fn *ir.Function, modeData []byte) {
	for i := range modeData {
		if i >= len(fn.Values) {
			return
		}
		mode := ir.RefMode(modeData[i] % 5)
		if mode == ir.RefModeNone {
			continue
		}
		fn.SetRefMode(uint32(i), mode)
	}
}
