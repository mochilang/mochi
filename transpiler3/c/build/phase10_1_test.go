package build

import (
	"runtime"
	"testing"
)

// TestPhase10BoxedValue is the MEP-45 Phase 10.1 gate.
//
// Phase 10.1 adds the mochi_value_t boxed type to the transpiler3/c
// pipeline:
//   - TypeValue added to aotir.Type; "value" recognized by typeFromRef.
//   - cType maps TypeValue to mochi_value_t in the C emitter.
//   - runtime/include/mochi/value.h defines the mochi_value_t tagged
//     union (nil, bool, int, float, str, handle) and all marshalling
//     helpers.
//   - runtime/src/value.c implements the marshalling helpers.
//   - The prologue unconditionally includes mochi/value.h.
//
// This test runs the ffi_value fixture suite (8 fixtures) and asserts
// each binary's stdout matches its expect.txt.
func TestPhase10BoxedValue(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 10.1 gate skipped on Windows; Phase 11 wires Windows CI")
	}
	runFixtureSuite(t, "ffi_value")
}
