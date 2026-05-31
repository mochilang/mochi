package build

import (
	"testing"
)

// TestPhase2Scalars is the MEP-54 Phase 2 gate for the Go transpiler.
//
// Phase 2 lowering contract:
//
//   - int → int64; arithmetic (+, -, *, /, %) all use int64.
//     Integer division by zero is guarded: emits a deferred recover
//     that prints "MOCHI_ERR_DIVZERO\n" and exits 0.
//
//   - float → float64; NaN/±Inf edge cases match vm3 via
//     strconv.FormatFloat with format 'g', bitSize 64.
//
//   - bool → bool; && / || / ! lower directly.
//
//   - string → string; concatenation via +; string(int) for codepoint
//     conversion; strconv for int_to_str / float_to_str / bool_to_str.
//
//   - if/else → Go if/else.
//
//   - while COND { body } → for COND { body }.
//
//   - for i in range(lo, hi) { body }
//     → for i := lo; i < hi; i++ { body }.
//
//   - User functions: top-level Go func declarations; mutual recursion
//     allowed (Go forward-declares all functions at package scope).
//
//   - Logical ops and comparison: ==, !=, <, <=, >, >= lower directly.
func TestPhase2Scalars(t *testing.T) {
	runFixturesMatchingPrefixes(t,
		"int_", "float_", "bool_",
		"string_", "str_bool", "str_concat", "str_float", "str_int",
		"str_in_condition", "str_in_function", "str_string_identity",
		"if_", "while_", "for_range",
		"divzero", "nan_",
		"user_fn_",
		"fun_",
		"var_let",
	)
}
